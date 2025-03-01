module Data.RuleSetups exposing
    ( collisionSetupCollided
    , collisionSetupNearCollision
    , collisionSetupPathsIntersect
    , connectedRoadsSetup
    , greenTrafficLightsSetup
    , largeWorldSetup
    , noCollisionSetupDifferentLanes
    , noCollisionSetupIntersection
    , redTrafficLightsSetup
    , routeVisualizationSetup
    , yieldSlowDownSetup
    , yieldWithPriorityTrafficSetup1
    , yieldWithPriorityTrafficSetup2
    , yieldWithoutPriorityTrafficSetup
    )

import Angle exposing (Angle)
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (testCar)
import Data.Worlds
    exposing
        ( disconnectedWorld
        , largeWorld
        , simpleWorld
        , worldWithFourWayIntersection
        , worldWithThreeWayIntersection
        )
import Duration
import Length exposing (Length)
import Lib.Collection as Collection exposing (Id)
import Model.World as World exposing (World)
import Point2d exposing (Point2d)
import Random
import Simulation.Car as Car exposing (Car)
import Simulation.Events exposing (updateEventQueue)
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Simulation.Route as Route
import Simulation.Steering as Steering
import Simulation.Traffic as Traffic exposing (RuleSetup)
import Speed exposing (Speed)
import Time


connectedRoadsSetup : RuleSetup
connectedRoadsSetup =
    let
        world =
            simpleWorld

        delta =
            Duration.milliseconds 200

        ( car, _ ) =
            buildCar CarA1 (Point2d.meters 8 150) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 8 150)
                |> Traffic.updateCar delta world

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car
    in
    RuleSetup worldWithCars car otherCars


collisionSetupPathsIntersect : RuleSetup
collisionSetupPathsIntersect =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 16 134) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 16 134
                    , Point2d.meters 26 144
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 4)

                Nothing ->
                    car

        otherCar =
            buildCar CarB2 (Point2d.meters 32 138) (Angle.degrees 180) Steering.maxVelocity

        routedOtherCar =
            case
                positionsToNodes world
                    [ Point2d.meters 32 138
                    , Point2d.meters 16 138
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    otherCar |> routeCarByNodes startNode otherNodes (Length.meters 4)

                Nothing ->
                    otherCar

        otherCars =
            [ routedOtherCar
            ]

        worldWithCars =
            world
                |> World.setCar routedCar
                |> World.setCar routedOtherCar
    in
    RuleSetup worldWithCars routedCar otherCars


collisionSetupNearCollision : RuleSetup
collisionSetupNearCollision =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 16 134) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 16 134
                    , Point2d.meters 26 144
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 6)

                Nothing ->
                    car

        otherCar =
            buildCar CarB2 (Point2d.meters 32 138) (Angle.degrees 180) Steering.maxVelocity

        routedOtherCar =
            case
                positionsToNodes world
                    [ Point2d.meters 32 138
                    , Point2d.meters 16 138
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    otherCar |> routeCarByNodes startNode otherNodes (Length.meters 6)

                Nothing ->
                    otherCar

        otherCars =
            [ routedOtherCar
            ]

        worldWithCars =
            world
                |> World.setCar routedCar
                |> World.setCar routedOtherCar
    in
    RuleSetup worldWithCars routedCar otherCars


collisionSetupCollided : RuleSetup
collisionSetupCollided =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 16 134) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 16 134
                    , Point2d.meters 26 144
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 7)

                Nothing ->
                    car

        otherCar =
            buildCar CarB2 (Point2d.meters 32 138) (Angle.degrees 180) Steering.maxVelocity

        routedOtherCar =
            case
                positionsToNodes world
                    [ Point2d.meters 32 138
                    , Point2d.meters 16 138
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    otherCar |> routeCarByNodes startNode otherNodes (Length.meters 6.5)

                Nothing ->
                    otherCar

        otherCars =
            [ routedOtherCar
            ]

        worldWithCars =
            world
                |> World.setCar routedCar
                |> World.setCar routedOtherCar
    in
    RuleSetup worldWithCars routedCar otherCars


noCollisionSetupDifferentLanes : RuleSetup
noCollisionSetupDifferentLanes =
    let
        world =
            simpleWorld

        delta =
            Duration.milliseconds 500

        ( car, _ ) =
            buildCar CarA1 (Point2d.meters 8 150) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 8 150)
                |> Traffic.updateCar delta world

        ( otherCar, _ ) =
            buildCar CarB2 (Point2d.meters 24 154) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 24 154)
                |> Traffic.updateCar delta world

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car
                |> World.setCar otherCar
    in
    RuleSetup worldWithCars car otherCars


noCollisionSetupIntersection : RuleSetup
noCollisionSetupIntersection =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 16 134) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 16 134)

        otherCar =
            buildCar CarB2 (Point2d.meters 26 128) (Angle.degrees 90) Steering.maxVelocity

        routedOtherCar =
            case
                positionsToNodes world
                    [ Point2d.meters 26 128
                    , Point2d.meters 26 144
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    otherCar |> routeCarByNodes startNode otherNodes (Length.meters 10)

                Nothing ->
                    otherCar

        otherCars =
            [ routedOtherCar
            ]

        worldWithCars =
            world
                |> World.setCar car
                |> World.setCar routedOtherCar
    in
    RuleSetup worldWithCars car otherCars


redTrafficLightsSetup : RuleSetup
redTrafficLightsSetup =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 26 120) (Angle.degrees 90) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 26 120)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car
    in
    RuleSetup worldWithCars car otherCars


greenTrafficLightsSetup : RuleSetup
greenTrafficLightsSetup =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 8 134) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 8 134)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car
    in
    RuleSetup worldWithCars car otherCars


yieldWithPriorityTrafficSetup1 : RuleSetup
yieldWithPriorityTrafficSetup1 =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 8 118) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 8 118
                    , Point2d.meters 32 118
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 22)

                Nothing ->
                    car

        otherCar =
            buildCar CarB2 (Point2d.meters 42 108.4) (Angle.degrees 90) Steering.maxVelocity

        otherCars =
            [ otherCar ]

        worldWithCars =
            world
                |> World.setCar routedCar
                |> World.setCar otherCar
    in
    RuleSetup worldWithCars routedCar otherCars


yieldWithPriorityTrafficSetup2 : RuleSetup
yieldWithPriorityTrafficSetup2 =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 8 118) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 8 118
                    , Point2d.meters 32 118
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 22)

                Nothing ->
                    car

        otherCar =
            buildCar CarB2 (Point2d.meters 38 128.4) (Angle.degrees 270) Steering.maxVelocity

        otherCars =
            [ otherCar ]

        worldWithCars =
            world
                |> World.setCar routedCar
                |> World.setCar otherCar
    in
    RuleSetup worldWithCars routedCar otherCars


yieldWithoutPriorityTrafficSetup : RuleSetup
yieldWithoutPriorityTrafficSetup =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 8 118) (Angle.degrees 0) (Speed.metersPerSecond 0)

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 8 118
                    , Point2d.meters 32 118
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 20)

                Nothing ->
                    car

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar routedCar
    in
    RuleSetup worldWithCars routedCar otherCars


yieldSlowDownSetup : RuleSetup
yieldSlowDownSetup =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 8 118) (Angle.degrees 0) Steering.maxVelocity

        routedCar =
            case
                positionsToNodes world
                    [ Point2d.meters 8 118
                    , Point2d.meters 32 118
                    ]
            of
                Just ( startNode, otherNodes ) ->
                    car |> routeCarByNodes startNode otherNodes (Length.meters 18)

                Nothing ->
                    car

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar routedCar
    in
    RuleSetup worldWithCars routedCar otherCars


largeWorldSetup : Int -> RuleSetup
largeWorldSetup carsAmount =
    let
        world =
            largeWorld

        worldWithCars =
            spawnCars carsAmount world
                |> updateEventQueue (Time.millisToPosix 16)
    in
    case Collection.values worldWithCars.cars of
        x :: xs ->
            RuleSetup worldWithCars x xs

        [] ->
            -- Dummy value, should never get here
            connectedRoadsSetup


routeVisualizationSetup : RuleSetup
routeVisualizationSetup =
    let
        world =
            disconnectedWorld

        car =
            buildCar CarA1 (Point2d.meters 8 6) (Angle.degrees 0) Steering.maxVelocity

        worldWithCars =
            world
                |> World.setCar car
    in
    RuleSetup worldWithCars car []



--
-- Utility
--


type TestCar
    = CarA1
    | CarB2


id1 : Id
id1 =
    Collection.initialId


id2 : Id
id2 =
    Collection.nextId id1


buildCar : TestCar -> Point2d Length.Meters GlobalCoordinates -> Angle -> Speed -> Car
buildCar option position orientation velocity =
    let
        id =
            case option of
                CarA1 ->
                    id1

                CarB2 ->
                    id2
    in
    Car.new testCar
        |> Car.withPosition position
        |> Car.withOrientation orientation
        |> Car.withVelocity velocity
        |> Car.build Nothing
        |> (\builderFn -> builderFn id)


routeCarByDestination : World -> Point2d Length.Meters GlobalCoordinates -> Car -> Car
routeCarByDestination world position car =
    let
        destination =
            RoadNetwork.nodeByPosition world.roadNetwork position
    in
    case destination of
        Just nodeCtx ->
            let
                -- Ignore seed: test setup
                ( route, _ ) =
                    Route.randomFromNode
                        (Random.initialSeed 666)
                        2
                        world.roadNetwork
                        nodeCtx
            in
            car
                |> Car.routed route
                |> Traffic.applySteering (Duration.milliseconds 16) Steering.none

        Nothing ->
            car


positionsToNodes : World -> List (Point2d Length.Meters GlobalCoordinates) -> Maybe ( RNNodeContext, List RNNodeContext )
positionsToNodes world nodePositions =
    case
        nodePositions
            |> List.filterMap (RoadNetwork.nodeByPosition world.roadNetwork)
    of
        firstNode :: others ->
            Just ( firstNode, others )

        _ ->
            Nothing


routeCarByNodes : RNNodeContext -> List RNNodeContext -> Length -> Car -> Car
routeCarByNodes node others parameter car =
    let
        route =
            Route.fromNodesAndParameter
                node
                others
                parameter
    in
    { car | route = route }
        |> Traffic.applySteering (Duration.milliseconds 16) Steering.none


spawnCars : Int -> World -> World
spawnCars n world =
    if n == 0 then
        world

    else
        let
            ( nextWorld, _ ) =
                Traffic.spawnTestCar world
        in
        spawnCars
            (n - 1)
            nextWorld
