module Simulation.Traffic exposing
    ( Rule(..)
    , RuleSetup
    , addLotResident
    , applySteering
    , checkRules
    , rerouteCarsIfNeeded
    , spawnCar
    , updateTraffic
    )

import BoundingBox2d
import Common
import Data.Cars exposing (CarMake)
import Data.Lots
import Dict
import Direction2d
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Entity as Entity exposing (Id)
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d, orthogonalDirectionToLmDirection)
import Model.Lookup exposing (carPositionLookup)
import Model.Lot as Lot exposing (Lot, ParkingSpot)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( RNNodeContext
        , TrafficControl(..)
        )
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
import Speed


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 12


yieldSlowDownDistance : Length
yieldSlowDownDistance =
    Length.meters 12


yieldReactionDistance : Length
yieldReactionDistance =
    Length.meters 4


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 4


parkingRadius : Length
parkingRadius =
    Length.meters 8


nearbyTrafficRadius : Length
nearbyTrafficRadius =
    Length.meters 16


routeEndSlowRadius : Length
routeEndSlowRadius =
    Length.meters 15


updateTraffic :
    { updateQueue : List Car
    , seed : Random.Seed
    , world : World
    , roadNetworkStale : Bool
    , delta : Duration
    }
    -> World
updateTraffic { updateQueue, seed, world, delta, roadNetworkStale } =
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
                            carAfterDespawn seed world carWithUpdatedFSM

                        _ ->
                            Just
                                (Pathfinding.carAfterRouteUpdate
                                    seed
                                    world
                                    roadNetworkStale
                                    delta
                                    carWithUpdatedFSM
                                )

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
                , roadNetworkStale = roadNetworkStale
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
        car.parkingReservation
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
                    let
                        carWithUnparkingOrientation =
                            { car
                                | orientation =
                                    lot.parkingSpotExitDirection
                                        |> orthogonalDirectionToLmDirection
                                        |> Direction2d.toAngle
                            }
                    in
                    world
                        |> World.setCar carWithUnparkingOrientation
                        |> World.setLot (Lot.releaseParkingLock carWithUnparkingOrientation.id lot)

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
                    World.setLot nextLot world

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


carAfterDespawn : Random.Seed -> World -> Car -> Maybe Car
carAfterDespawn seed world car =
    car.homeLotId
        |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)
        |> Maybe.andThen (\lot -> Lot.findFreeParkingSpot car.id lot |> Maybe.map (Tuple.pair lot))
        |> Maybe.map (moveCarToHome seed world car)


moveCarToHome : Random.Seed -> World -> Car -> ( Lot, ParkingSpot ) -> Car
moveCarToHome seed world car ( home, parkingSpot ) =
    case
        car.homeLotId
    of
        Just _ ->
            let
                parkingReservation =
                    { lotId = home.id
                    , parkingSpotId = parkingSpot.id
                    }

                ( nextFSM, _ ) =
                    FSM.reset car.fsm
            in
            { car
                | fsm = nextFSM
                , position = parkingSpot.position
                , orientation = Lot.parkingSpotOrientation home
                , velocity = Quantity.zero
                , parkingReservation = Just parkingReservation
                , route = Pathfinding.generateRouteFromParkingSpot seed world car.id parkingReservation
            }

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


addLotResident : Lot -> World -> World
addLotResident lot world =
    let
        carId =
            Entity.nextId world.cars

        carMake =
            Data.Lots.resident lot.kind lot.themeColor

        parkingSpot =
            Lot.claimParkingSpot carId lot
    in
    Maybe.map2
        (createResident world carId lot)
        carMake
        parkingSpot
        |> Maybe.withDefault world


createResident : World -> Id -> Lot -> CarMake -> ParkingSpot -> World
createResident world carId lot make parkingSpot =
    let
        lotWithClaimedParkingSpot =
            Lot.updateParkingSpot parkingSpot lot

        car =
            createCar
                carId
                lotWithClaimedParkingSpot
                make
                world
                parkingSpot
    in
    world
        |> World.setCar car
        |> World.setLot lotWithClaimedParkingSpot


createCar : Id -> Lot -> CarMake -> World -> ParkingSpot -> Car
createCar carId lot make world parkingSpot =
    let
        parkingReservation =
            { lotId = lot.id
            , parkingSpotId = parkingSpot.id
            }

        route =
            Pathfinding.generateRouteFromParkingSpot
                (Random.initialSeed (carId + lot.id))
                world
                carId
                parkingReservation
    in
    Car.new make
        |> Car.withHome lot.id
        |> Car.withPosition parkingSpot.position
        |> Car.withOrientation (Lot.parkingSpotOrientation lot)
        |> Car.build carId (Just route) (Just parkingReservation)



--
-- Spawn cars
--


spawnCar : Random.Seed -> World -> ( World, Random.Seed, Maybe Entity.Id )
spawnCar seed world =
    let
        ( maybeRandomNodeCtx, seedAfterRandomNode ) =
            RoadNetwork.getRandomNode world.roadNetwork
                seed
                (\node -> node.label.kind == RoadNetwork.LaneConnector)
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    id =
                        Entity.nextId world.cars

                    route =
                        Pathfinding.generateRouteFromNode seedAfterRandomNode world id nodeCtx

                    ( carMake, _ ) =
                        Random.step Data.Cars.randomCarMake seed

                    car =
                        Car.new carMake
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build id (Just route) Nothing
                in
                ( World.setCar car world
                , seedAfterRandomNode
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
    = AvoidCollision Length Bool
    | WaitForOtherCar
    | StopAtTrafficControl Length
    | SlowDownAtTrafficControl
    | SlowDownNearRouteEnd
    | WaitForParkingSpot
    | StopAtParkingSpot
    | StayParked


checkRules : RuleSetup -> Steering
checkRules setup =
    let
        activeRule =
            Maybe.orListLazy
                [ \() -> checkCollision setup
                , \() -> checkParking setup
                , \() -> checkTrafficControl setup
                , \() -> checkRoute setup
                ]
    in
    case activeRule of
        Just rule ->
            applyRule setup.activeCar rule

        Nothing ->
            -- cancel the effects of previously applied rules
            if Route.isRouted setup.activeCar.route then
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
        AvoidCollision distanceToCollision collisionBehindCar ->
            let
                carBumperDistance =
                    activeCar.make.length
                        |> Quantity.half
                        |> Quantity.plus (Length.meters 0.1)
            in
            if distanceToCollision |> Quantity.lessThanOrEqualTo carBumperDistance then
                if collisionBehindCar then
                    Steering.stop velocity

                else
                    Steering.reactToCollision

            else
                Steering.stopAtDistance
                    distanceToCollision
                    (make.length |> Quantity.multiplyBy 1.5)
                    velocity

        WaitForOtherCar ->
            Steering.stop velocity

        StopAtTrafficControl distanceFromTrafficControl ->
            Steering.stopAtDistance
                distanceFromTrafficControl
                trafficControlStopDistance
                velocity

        SlowDownAtTrafficControl ->
            Steering.goSlow velocity Nothing

        SlowDownNearRouteEnd ->
            Steering.goSlow
                velocity
                (Just <| Speed.metersPerSecond 3.5)

        WaitForParkingSpot ->
            Steering.stop velocity

        StopAtParkingSpot ->
            Steering.stopAtPathEnd
                position
                velocity
                activeCar.route
                parkingRadius

        StayParked ->
            Steering.stop velocity



-- Rule definitions


checkCollision : RuleSetup -> Maybe Rule
checkCollision { activeCar, otherCars } =
    List.foldl
        (\otherCar currentRule ->
            let
                collisionResult =
                    Collision.checkFutureCollision activeCar otherCar
            in
            case collisionResult of
                Collision.PotentialCollision pointOfCollision ->
                    let
                        distanceToCollision =
                            Point2d.distanceFrom activeCar.position pointOfCollision

                        collisionBehindCar =
                            pointOfCollision
                                |> Common.isInTheNormalPlaneOf
                                    (Direction2d.fromAngle activeCar.orientation)
                                    activeCar.position
                                |> not

                        collisionRule =
                            AvoidCollision distanceToCollision collisionBehindCar
                    in
                    case currentRule of
                        Just (AvoidCollision smallestCollisionDistanceValue _) ->
                            if distanceToCollision |> Quantity.lessThan smallestCollisionDistanceValue then
                                -- Replace previous smallest value
                                Just collisionRule

                            else
                                -- Keep the old value
                                currentRule

                        _ ->
                            Just collisionRule

                Collision.Caution ->
                    Maybe.orLazy
                        currentRule
                        (\_ -> Just WaitForOtherCar)

                Collision.NoCollision ->
                    currentRule
        )
        Nothing
        otherCars


checkTrafficControl : RuleSetup -> Maybe Rule
checkTrafficControl setup =
    setup.activeCar.route
        |> Pathfinding.routeTrafficControl setup.world
        |> Maybe.andThen
            (\( trafficControl, position ) ->
                case trafficControl of
                    Signal id ->
                        let
                            { trafficLights } =
                                setup.world
                        in
                        Dict.get id trafficLights |> Maybe.andThen (checkTrafficLights setup)

                    Yield yieldCheckArea ->
                        checkYield setup position yieldCheckArea

                    NoTrafficControl ->
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


checkYield : RuleSetup -> LMPoint2d -> LMBoundingBox2d -> Maybe Rule
checkYield { activeCar, otherCars } signPosition checkArea =
    let
        distanceFromYieldSign =
            Point2d.distanceFrom activeCar.position signPosition
    in
    if
        (distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldReactionDistance)
            && List.any (hasPriority checkArea) otherCars
    then
        Just (StopAtTrafficControl distanceFromYieldSign)

    else if distanceFromYieldSign |> Quantity.lessThanOrEqualTo yieldSlowDownDistance then
        Just SlowDownAtTrafficControl

    else
        Nothing


hasPriority : LMBoundingBox2d -> Car -> Bool
hasPriority yieldCheckArea otherCar =
    let
        centerPoint =
            BoundingBox2d.centerPoint yieldCheckArea

        isInYieldCheckArea =
            BoundingBox2d.contains otherCar.position yieldCheckArea
    in
    isInYieldCheckArea
        && -- If the center point of the yield check area is in any way visible to the car,
           -- it must be heading to the intersection - it has priority
           (centerPoint
                |> Common.isInTheNormalPlaneOf
                    (Direction2d.fromAngle otherCar.orientation)
                    otherCar.position
           )


checkParking : RuleSetup -> Maybe Rule
checkParking { activeCar } =
    case FSM.toCurrentState activeCar.fsm of
        WaitingForParkingSpot ->
            Just WaitForParkingSpot

        Parking ->
            Just StopAtParkingSpot

        Parked ->
            Just StayParked

        _ ->
            Nothing


checkRoute : RuleSetup -> Maybe Rule
checkRoute { activeCar } =
    Route.distanceToPathEnd activeCar.route
        |> Maybe.andThen
            (\distanceToPathEnd ->
                if distanceToPathEnd |> Quantity.lessThanOrEqualTo routeEndSlowRadius then
                    Just SlowDownNearRouteEnd

                else
                    Nothing
            )
