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
import Data.Cars exposing (testCar)
import Data.Worlds
    exposing
        ( disconnectedWorld
        , largeWorld
        , simpleWorld
        , worldWithFourWayIntersection
        , worldWithThreeWayIntersection
        )
import Dict
import Duration
import Length exposing (Length)
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Model.Route as Route
import Model.World as World exposing (World)
import Point2d
import Random
import Simulation.Steering as Steering
import Simulation.Traffic as Traffic exposing (RuleSetup)
import Speed exposing (Speed)


connectedRoadsSetup : RuleSetup
connectedRoadsSetup =
    let
        world =
            simpleWorld

        car =
            buildCar CarA1 (Point2d.meters 10 150) (Angle.degrees 0) Steering.maxVelocity

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

        car =
            buildCar CarA1 (Point2d.meters 12 150) (Angle.degrees 0) Steering.maxVelocity

        otherCar =
            buildCar CarB2 (Point2d.meters 20 153.8) (Angle.degrees 180) Steering.maxVelocity

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

        otherCar =
            buildCar CarB2 (Point2d.meters 26 138) (Angle.degrees 90) Steering.maxVelocity

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car
                |> World.setCar otherCar
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
            spawnCars carsAmount world (Random.initialSeed 888)
    in
    case Dict.values worldWithCars.cars of
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


buildCar : TestCar -> LMPoint2d -> Angle -> Speed -> Car
buildCar option position orientation velocity =
    let
        id =
            case option of
                CarA1 ->
                    1

                CarB2 ->
                    2
    in
    Car.new testCar
        |> Car.withPosition position
        |> Car.withOrientation orientation
        |> Car.withVelocity velocity
        -- TODO: combine routeCarByDestination with buildCar for easier setup
        |> Car.build id Nothing


routeCarByDestination : World -> LMPoint2d -> Car -> Car
routeCarByDestination world position car =
    let
        destination =
            RoadNetwork.findNodeByPosition world.roadNetwork position
    in
    case destination of
        Just nodeCtx ->
            let
                ( route, _ ) =
                    Route.randomFromNode
                        (Random.initialSeed 666)
                        2
                        world.roadNetwork
                        nodeCtx
            in
            { car | route = route }
                |> Traffic.applySteering (Duration.milliseconds 16) Steering.none

        Nothing ->
            car


positionsToNodes : World -> List LMPoint2d -> Maybe ( RNNodeContext, List RNNodeContext )
positionsToNodes world nodePositions =
    case
        nodePositions
            |> List.filterMap (RoadNetwork.findNodeByPosition world.roadNetwork)
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


spawnCars : Int -> World -> Random.Seed -> World
spawnCars n world aSeed =
    if n == 0 then
        world

    else
        let
            ( nextWorld, nextSeed, _ ) =
                Traffic.spawnCar aSeed world
        in
        spawnCars (n - 1) nextWorld nextSeed
