module Simulation.Traffic exposing
    ( Rule(..)
    , RuleSetup
    , addLotResident
    , applySteering
    , checkRules
    , rerouteCarsIfNeeded
    , spawnResident
    , spawnTestCar
    , updateCar
    , updateTraffic
    )

import BoundingBox2d
import Collection
import Common exposing (randomFutureTime)
import Data.Cars exposing (CarMake)
import Data.Lots
import Direction2d
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d)
import Model.Lot as Lot exposing (Lot)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( RNNodeContext
        , TrafficControl(..)
        )
import Model.Route as Route
import Model.TrafficLight as TrafficLight exposing (TrafficLight)
import Model.World as World exposing (World, WorldEvent)
import Point2d
import Quantity
import Random
import Simulation.Collision as Collision
import Simulation.Pathfinding as Pathfinding
import Simulation.Steering as Steering exposing (Steering)
import Speed
import Time


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


stopAtNodeRadius : Length
stopAtNodeRadius =
    Length.meters 8


nearbyTrafficRadius : Length
nearbyTrafficRadius =
    Length.meters 16


routeEndSlowRadius : Length
routeEndSlowRadius =
    Length.meters 15


updateTraffic :
    { updateQueue : List Car
    , world : World
    , delta : Duration
    , events : List WorldEvent
    }
    -> ( World, List WorldEvent )
updateTraffic { updateQueue, world, delta, events } =
    case updateQueue of
        [] ->
            ( { world | carLookup = World.createLookup (Collection.values world.cars) world }
            , events
            )

        activeCar :: queue ->
            let
                ( nextCar, worldEvents ) =
                    updateCar delta world activeCar
            in
            updateTraffic
                { updateQueue = queue
                , world = World.setCar nextCar world
                , delta = delta
                , events = events ++ worldEvents
                }


updateCar : Duration -> World -> Car -> ( Car, List World.WorldEvent )
updateCar delta world activeCar =
    let
        fsmUpdateContext =
            { currentPosition = activeCar.position
            , currentVelocity = activeCar.velocity
            , route = activeCar.route
            }

        ( nextFSM, fsmEvents ) =
            FSM.update delta fsmUpdateContext activeCar.fsm

        carWithFsmUpdate =
            { activeCar | fsm = nextFSM }

        ( nextRoute, pathfindingEvent ) =
            Pathfinding.updateRoute delta carWithFsmUpdate

        otherCars =
            world.carLookup
                |> World.findNearbyEntities nearbyTrafficRadius carWithFsmUpdate.boundingBox
                |> List.filter (\car -> car.id /= carWithFsmUpdate.id)

        steering =
            checkRules
                { world = world
                , otherCars = otherCars
                , activeCar = carWithFsmUpdate
                }

        worldEvents =
            pathfindingEvent :: List.map (World.CarStateChange carWithFsmUpdate.id) fsmEvents
    in
    ( applySteering
        delta
        steering
        { carWithFsmUpdate | route = nextRoute }
    , worldEvents
    )


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


rerouteCarsIfNeeded : World -> World
rerouteCarsIfNeeded world =
    { world
        | cars =
            Collection.map
                (\_ car -> Pathfinding.restoreRoute world car)
                world.cars
    }



--
-- Spawn cars
--


addLotResident : Time.Posix -> Lot -> World -> World
addLotResident time lot world =
    case Data.Lots.resident lot.kind lot.themeColor of
        Just carMake ->
            let
                ( triggerAt, nextSeed ) =
                    Random.step
                        (randomFutureTime ( 2500, 10000 ) time)
                        world.seed
            in
            world
                |> World.setSeed nextSeed
                |> World.addEvent
                    (World.SpawnResident carMake lot.id)
                    triggerAt

        Nothing ->
            world


spawnResident : Time.Posix -> CarMake -> Lot -> World -> Result String World
spawnResident time carMake lot world =
    Lot.findFreeParkingSpot Lot.parkingSpotEligibleForResident lot
        |> Result.fromMaybe "Could not find free parking spot"
        |> Result.map
            (\parkingSpot ->
                let
                    builderFn =
                        Car.new carMake
                            |> Car.withHome lot.id
                            |> Car.withPosition parkingSpot.position
                            |> Car.withOrientation (Lot.parkingSpotOrientation lot)
                            |> Car.build (Just parkingReservation)

                    ( car, nextCars ) =
                        Collection.addFromBuilder builderFn world.cars

                    lotWithReservedParkingSpot =
                        Lot.reserveParkingSpot car.id parkingSpot.id lot

                    parkingReservation =
                        { lotId = lot.id
                        , parkingSpotId = parkingSpot.id
                        }

                    ( routeTriggerAt, nextSeed ) =
                        Random.step
                            (randomFutureTime ( 1000, 20000 ) time)
                            world.seed
                in
                world
                    |> World.refreshCars nextCars
                    |> World.updateLot lotWithReservedParkingSpot
                    |> World.setSeed nextSeed
                    |> World.addEvent
                        (World.CreateRouteFromParkingSpot car.id parkingReservation)
                        routeTriggerAt
            )


spawnTestCar : World -> ( World, Maybe Collection.Id )
spawnTestCar world =
    let
        maybeRandomNodeCtx =
            RoadNetwork.getRandomNode
                world.roadNetwork
                world.seed
                (\node -> node.label.kind == RoadNetwork.LaneConnector)
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    ( carMake, nextSeed ) =
                        Random.step Data.Cars.randomCarMake world.seed

                    builderFn =
                        Car.new carMake
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build Nothing

                    ( car, nextCars ) =
                        Collection.addFromBuilder builderFn world.cars
                in
                ( world
                    |> World.refreshCars nextCars
                    |> World.setSeed nextSeed
                    |> World.addEvent
                        (World.CreateRouteFromNode car.id nodeCtx)
                        (Time.millisToPosix 0)
                , Just car.id
                )
            )
        |> Maybe.withDefault ( world, Nothing )


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            car.boundingBox
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            RoadNetwork.size world.roadNetwork > Collection.size world.cars

        spawnPositionHasEnoughSpace =
            Collection.values world.cars
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
    | StopAtRouteEnd Length
    | WaitForParkingSpot
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
                Steering.stop setup.activeCar.velocity


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

        StopAtRouteEnd stopRadius ->
            Steering.stopAtPathEnd
                position
                velocity
                activeCar.route
                stopRadius

        WaitForParkingSpot ->
            Steering.stop velocity

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
                        Collection.get id trafficLights |> Maybe.andThen (checkTrafficLights setup)

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
    Car.isMoving otherCar
        && BoundingBox2d.contains otherCar.position yieldCheckArea
        && -- If the center point of the yield check area is in any way visible to the car,
           -- it must be heading to the intersection - it has priority
           (yieldCheckArea
                |> BoundingBox2d.centerPoint
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
            Just (StopAtRouteEnd parkingRadius)

        Parked ->
            Just StayParked

        Despawning ->
            Just (StopAtRouteEnd stopAtNodeRadius)

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
