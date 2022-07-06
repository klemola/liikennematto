module Simulation.Traffic exposing
    ( Rule(..)
    , RuleSetup
    , addLotResident
    , checkRules
    , rerouteCarsIfNeeded
    , spawnCar
    , updateTraffic
    )

import BoundingBox2d
import Data.Cars exposing (CarMake, testCar)
import Data.Lots
import Dict
import Direction2d
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Entity as Entity exposing (Id)
import Model.Geometry exposing (LMPoint2d)
import Model.Lookup exposing (carPositionLookup)
import Model.Lot as Lot exposing (Lot, ParkingSpot)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext, TrafficControl(..))
import Model.Route as Route
import Model.TrafficLight as TrafficLight exposing (TrafficLight)
import Model.World as World exposing (World)
import Point2d
import QuadTree
import Quantity
import Random
import Simulation.Collision as Collision
import Simulation.Pathfinding as Pathfinding
import Simulation.Steering as Steering exposing (Steering)


maxCarCollisionTestDistance : Length
maxCarCollisionTestDistance =
    Length.meters 16


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 32


yieldSlowDownDistance : Length
yieldSlowDownDistance =
    Length.meters 16


yieldReactionDistance : Length
yieldReactionDistance =
    Length.meters 5


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 4


parkingRadius : Length
parkingRadius =
    Length.meters 8


nearbyTrafficRadius : Length
nearbyTrafficRadius =
    Length.meters 16


initialParkingWaitTimer : Maybe Duration
initialParkingWaitTimer =
    Just (Duration.milliseconds 1500)


updateTraffic :
    { updateQueue : List Car
    , seed : Random.Seed
    , world : World
    , delta : Duration
    }
    -> World
updateTraffic { updateQueue, seed, world, delta } =
    case updateQueue of
        [] ->
            { world | carPositionLookup = carPositionLookup world.tilemap world.cars }

        activeCar :: queue ->
            let
                fsmUpdateContext =
                    { currentPosition = activeCar.position
                    , route = activeCar.route
                    }

                ( nextFSM, actions ) =
                    FSM.update delta fsmUpdateContext activeCar.fsm

                carWithUpdatedFSM =
                    { activeCar | fsm = nextFSM }

                otherCars =
                    world.carPositionLookup
                        |> QuadTree.neighborsWithin nearbyTrafficRadius activeCar.boundingBox
                        |> List.filter (\car -> car.id /= activeCar.id)

                steering =
                    checkRules
                        { world = world
                        , otherCars = otherCars
                        , activeCar = activeCar
                        }

                carAfterRouteUpdate =
                    case FSM.toCurrentState nextFSM of
                        Car.Despawning ->
                            -- Route update would just trigger despawn again, skip it
                            Just carWithUpdatedFSM

                        Car.Despawned ->
                            -- Route update not required, the car will be despawned after this update cycle
                            carAfterDespawn world carWithUpdatedFSM

                        _ ->
                            Just (updateRoute world delta seed carWithUpdatedFSM)

                nextWorld =
                    carAfterRouteUpdate
                        |> Maybe.map (applySteering delta steering)
                        |> Maybe.map (\updatedCar -> World.setCar updatedCar world)
                        |> Maybe.withDefaultLazy (\_ -> World.removeCar activeCar.id world)
                        -- The car might already be deleted, but the actions still need to be applied (using the car data on FSM update)
                        |> applyActions actions carWithUpdatedFSM
            in
            updateTraffic
                { updateQueue = queue
                , seed = seed
                , world = nextWorld
                , delta = delta
                }


applyActions : List Car.Action -> Car -> World -> World
applyActions actions car world =
    List.foldl (applyAction car)
        world
        actions


applyAction : Car -> Car.Action -> World -> World
applyAction car action world =
    case
        Route.parking car.route
            |> Maybe.andThen
                (\{ lotId, parkingSpotId } ->
                    Dict.get lotId world.lots
                        |> Maybe.map (Tuple.pair parkingSpotId)
                )
    of
        Just ( parkingSpotId, lot ) ->
            case action of
                Car.TriggerParkingStartEffects ->
                    lot
                        |> Lot.acquireParkingLock car.id
                        |> Maybe.map (Lot.reserveParkingSpot car.id parkingSpotId)
                        |> Maybe.map (\updatedLot -> World.setLot updatedLot world)
                        |> Maybe.withDefault world

                Car.TriggerParkingCompletedEffects ->
                    world
                        |> World.setCar (car |> Pathfinding.leaveLot world)
                        |> World.setLot (Lot.releaseParkingLock car.id lot)

                Car.TriggerUnparkingStartEffects ->
                    case Lot.acquireParkingLock car.id lot of
                        Just lotWithLock ->
                            World.setLot lotWithLock world

                        Nothing ->
                            -- The parking lock should have been free but was not
                            -- Room for improvement: acquire the parking lock when before unparking
                            world
                                |> World.setCar (Car.triggerDespawn car)
                                |> World.setLot (Lot.unreserveParkingSpot parkingSpotId lot)

                Car.TriggerUnparkingCompletedEffects ->
                    let
                        nextLot =
                            lot
                                |> Lot.releaseParkingLock car.id
                                |> Lot.unreserveParkingSpot parkingSpotId
                    in
                    world
                        |> World.setLot nextLot
                        |> World.setCar { car | route = Route.clearParking car.route }

        Nothing ->
            world


applySteering : Duration -> Steering -> Car -> Car
applySteering delta steering car =
    let
        ( pointOnSpline, tangentDirection ) =
            Route.sample car.route
                |> Maybe.withDefault ( car.position, Direction2d.fromAngle car.orientation )

        nextOrientation =
            Direction2d.toAngle tangentDirection

        nextVelocity =
            case steering.linear of
                Just acceleration ->
                    car.velocity
                        |> Quantity.plus (acceleration |> Quantity.for delta)
                        |> Steering.clampVelocity

                Nothing ->
                    car.velocity

        nextRotation =
            case steering.angular of
                Just angularAcceleration ->
                    car.rotation |> Quantity.plus (angularAcceleration |> Quantity.for delta)

                Nothing ->
                    Quantity.zero

        ( nextShape, nextBoundingBox ) =
            Car.adjustedShape car.make pointOnSpline nextOrientation
    in
    { car
        | position = pointOnSpline
        , orientation = nextOrientation
        , velocity = nextVelocity
        , rotation = nextRotation
        , shape = nextShape
        , boundingBox = nextBoundingBox
    }


updateRoute : World -> Duration -> Random.Seed -> Car -> Car
updateRoute world delta seed car =
    let
        ( nextRoute, _ ) =
            Pathfinding.updateRoute world delta seed car
    in
    case nextRoute of
        Route.Unrouted ->
            Car.triggerDespawn car

        Route.Parked _ ->
            { car | route = nextRoute }

        Route.Routed _ ->
            let
                parkingChanged =
                    Route.parking car.route == Nothing && Route.parking nextRoute /= Nothing

                routedCar =
                    if parkingChanged then
                        Car.triggerParking car nextRoute

                    else
                        { car | route = nextRoute }
            in
            routedCar


carAfterDespawn : World -> Car -> Maybe Car
carAfterDespawn world car =
    car.homeLotId
        |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)
        |> Maybe.andThen (\lot -> Lot.findFreeParkingSpot car.id lot |> Maybe.map (Tuple.pair lot))
        |> Maybe.map (moveCarToHome world car)


moveCarToHome : World -> Car -> ( Lot, ParkingSpot ) -> Car
moveCarToHome world car ( home, parkingSpot ) =
    case
        car.homeLotId
            |> Maybe.andThen (RoadNetwork.findLotExitNodeByLotId world.roadNetwork)
    of
        Just nodeCtx ->
            let
                parking =
                    { lotId = home.id
                    , parkingSpotId = parkingSpot.id
                    , waitTimer = initialParkingWaitTimer
                    , lockAvailable = False
                    }

                ( nextFSM, _ ) =
                    FSM.reset car.fsm
            in
            { car
                | fsm = nextFSM
                , position = parkingSpot.position
                , orientation = Lot.parkingSpotOrientation home
                , velocity = Quantity.zero
            }
                |> Pathfinding.resetCarAtLot parking nodeCtx parkingSpot

        _ ->
            car


rerouteCarsIfNeeded : World -> World
rerouteCarsIfNeeded world =
    let
        nextCars =
            Dict.map (\_ car -> Pathfinding.restoreRoute world car) world.cars
    in
    { world | cars = nextCars }



--
-- Cars & Lots
--


addLotResident : Id -> Lot -> World -> World
addLotResident lotId lot world =
    let
        carId =
            Entity.nextId world.cars

        -- Room for improvement: create the route and path after a resident is spawned so that
        -- a resident can be added before the road network is updated
        homeNode =
            RoadNetwork.findLotExitNodeByLotId world.roadNetwork lotId

        carMake =
            Data.Lots.resident lot.kind

        parkingSpot =
            Lot.claimParkingSpot carId lot
    in
    Maybe.map3
        (createResident world carId lot)
        homeNode
        carMake
        parkingSpot
        |> Maybe.withDefault world


createResident : World -> Id -> Lot -> RNNodeContext -> CarMake -> ParkingSpot -> World
createResident world carId lot homeNode make parkingSpot =
    let
        lotWithClaimedParkingSpot =
            Lot.updateParkingSpot parkingSpot lot

        car =
            createCar carId lotWithClaimedParkingSpot make homeNode parkingSpot
    in
    world
        |> World.setCar car
        |> World.setLot lotWithClaimedParkingSpot


createCar : Id -> Lot -> CarMake -> RNNodeContext -> ParkingSpot -> Car
createCar carId lot make homeNode parkingSpot =
    let
        parking =
            { lotId = lot.id
            , parkingSpotId = parkingSpot.id
            , waitTimer = initialParkingWaitTimer
            , lockAvailable = False
            }
    in
    Car.new make
        |> Car.withHome lot.id
        |> Car.withPosition parkingSpot.position
        |> Car.withOrientation (Lot.parkingSpotOrientation lot)
        |> Car.build carId
        |> Pathfinding.resetCarAtLot parking homeNode parkingSpot



--
-- Spawn cars
--


spawnCar : Random.Seed -> World -> ( World, Random.Seed, Maybe Entity.Id )
spawnCar seed world =
    let
        ( maybeRandomNodeCtx, seedAfterRandomNode ) =
            RoadNetwork.getRandomNode world.roadNetwork seed
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    id =
                        Entity.nextId world.cars

                    ( car, seedAfterRouteInit ) =
                        Car.new testCar
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build id
                            |> Pathfinding.setupRoute world seedAfterRandomNode nodeCtx
                in
                ( World.setCar car world
                , seedAfterRouteInit
                , Just id
                )
            )
        |> Maybe.withDefault ( world, seedAfterRandomNode, Nothing )


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            car.boundingBox
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            RoadNetwork.size world.roadNetwork > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing



--
-- Rules
--


type alias RuleSetup =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    }


type Rule
    = AvoidCollision Length
    | ReactToCollision
    | StopAtTrafficControl Length
    | SlowDownAtTrafficControl
    | StopAtParkingSpot
    | StayParked


checkRules : RuleSetup -> Steering
checkRules setup =
    let
        activeRule =
            Maybe.orListLazy
                [ \() -> checkForwardCollision setup
                , \() -> checkParking setup
                , \() -> checkTrafficControl setup
                , \() -> checkPathCollision setup
                ]
    in
    case activeRule of
        Just rule ->
            applyRule setup.activeCar rule

        Nothing ->
            -- cancel the effects of previously applied rules
            if Car.isPathfinding setup.activeCar then
                Steering.accelerate

            else
                Steering.none


applyRule : Car -> Rule -> Steering
applyRule activeCar rule =
    let
        { make, position, velocity } =
            activeCar
    in
    case rule of
        AvoidCollision distanceToCollision ->
            let
                collisionMargin =
                    make.length |> Quantity.multiplyBy 1.5

                targetDistance =
                    distanceToCollision |> Quantity.minus collisionMargin
            in
            if targetDistance |> Quantity.greaterThanZero then
                Steering.stopAtDistance
                    targetDistance
                    Quantity.zero
                    velocity

            else
                Steering.stop velocity

        ReactToCollision ->
            Steering.reactToCollision

        StopAtTrafficControl distanceFromTrafficControl ->
            Steering.stopAtDistance
                distanceFromTrafficControl
                trafficControlStopDistance
                velocity

        SlowDownAtTrafficControl ->
            Steering.goSlow velocity

        StopAtParkingSpot ->
            Steering.stopAtPathEnd
                position
                velocity
                activeCar.route
                parkingRadius

        StayParked ->
            Steering.stop velocity



-- Rule definitions


checkForwardCollision : RuleSetup -> Maybe Rule
checkForwardCollision { activeCar, otherCars } =
    let
        carFrontBumberDistance =
            activeCar.make.length
                |> Quantity.half
                |> Quantity.plus (Length.meters 0.1)

        ray =
            Collision.toRay activeCar maxCarCollisionTestDistance
    in
    Collision.distanceToClosestCollisionPoint
        activeCar
        otherCars
        (Collision.forwardCollisionWith ray activeCar)
        |> Maybe.map
            (\collisionDistance ->
                if collisionDistance |> Quantity.lessThanOrEqualTo carFrontBumberDistance then
                    ReactToCollision

                else
                    AvoidCollision collisionDistance
            )


checkPathCollision : RuleSetup -> Maybe Rule
checkPathCollision { activeCar, otherCars } =
    if Car.isStoppedOrWaiting activeCar then
        Nothing

    else
        let
            checkArea =
                Car.rightSideOfFieldOfView activeCar
        in
        Collision.distanceToClosestCollisionPoint
            activeCar
            otherCars
            (Collision.pathCollisionWith checkArea activeCar)
            |> Maybe.map AvoidCollision


checkTrafficControl : RuleSetup -> Maybe Rule
checkTrafficControl setup =
    Route.nextNode setup.activeCar.route
        |> Maybe.andThen
            (\{ node } ->
                case node.label.trafficControl of
                    Signal id ->
                        Dict.get id setup.world.trafficLights |> Maybe.andThen (checkTrafficLights setup)

                    Yield ->
                        checkYield setup node.label.position

                    None ->
                        Nothing
            )


checkTrafficLights : RuleSetup -> TrafficLight -> Maybe Rule
checkTrafficLights setup trafficLight =
    let
        distanceFromTrafficLight =
            Point2d.distanceFrom setup.activeCar.position trafficLight.position

        carShouldReact =
            distanceFromTrafficLight |> Quantity.lessThanOrEqualTo trafficLightReactionDistance
    in
    if TrafficLight.shouldStopTraffic trafficLight && carShouldReact then
        Just (StopAtTrafficControl distanceFromTrafficLight)

    else
        Nothing


checkYield : RuleSetup -> LMPoint2d -> Maybe Rule
checkYield { activeCar, otherCars } signPosition =
    let
        checkArea =
            Car.fieldOfView activeCar

        distanceFromYieldSign =
            Point2d.distanceFrom activeCar.position signPosition
    in
    if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldReactionDistance then
        Collision.distanceToClosestCollisionPoint
            activeCar
            otherCars
            (Collision.pathsIntersectAt checkArea activeCar)
            |> Maybe.map (always (StopAtTrafficControl distanceFromYieldSign))

    else if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldSlowDownDistance then
        Just SlowDownAtTrafficControl

    else
        Nothing


checkParking : RuleSetup -> Maybe Rule
checkParking { activeCar } =
    case FSM.toCurrentState activeCar.fsm of
        Parking ->
            Just StopAtParkingSpot

        Parked ->
            Just StayParked

        _ ->
            Nothing
