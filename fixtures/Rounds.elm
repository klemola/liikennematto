module Rounds exposing
    ( collisionSetupNearCollision
    , collisionSetupPathsIntersect
    , connectedRoadsSetup
    , greenTrafficLightsSetup
    , largeWorldSetup
    , noCollisionSetupDifferentLanes
    , noCollisionSetupIntersection
    , redTrafficLightsSetup
    , stopSetup
    , yieldAfterStopSetup
    , yieldWithPriorityTrafficSetup
    , yieldWithoutPriorityTrafficSetup
    )

import Angle exposing (Angle)
import Car exposing (Car)
import Dict
import Geometry
import Random
import RoadNetwork
import Round exposing (Round)
import Utility exposing (toLMPoint2d)
import World exposing (World)
import Worlds exposing (largeWorld, worldWithIntersection)


seed : Random.Seed
seed =
    Random.initialSeed 42


upIntersectionExitNodePosition : Geometry.LMPoint2d
upIntersectionExitNodePosition =
    toLMPoint2d 134 720


leftIntersectionExitNodePosition : Geometry.LMPoint2d
leftIntersectionExitNodePosition =
    toLMPoint2d 80 694


leftIntersectionEntryNodePosition : Geometry.LMPoint2d
leftIntersectionEntryNodePosition =
    toLMPoint2d 80 666


downIntersectionEntryNodePosition : Geometry.LMPoint2d
downIntersectionEntryNodePosition =
    toLMPoint2d 134 640


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 0, 720 ) (Angle.degrees 0)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


collisionSetupPathsIntersect : Round
collisionSetupPathsIntersect =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            buildCar CarA1 ( 102, 668 ) (Angle.degrees 30)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            buildCar CarB2 ( 160, 691 ) (Angle.degrees 180)

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx otherCar

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
                |> World.setCar otherCarWithRoute.id otherCarWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


collisionSetupNearCollision : Round
collisionSetupNearCollision =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork upIntersectionExitNodePosition

        car =
            buildCar CarA1 ( 110, 670 ) (Angle.degrees 45)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCarDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionExitNodePosition

        otherCar =
            buildCar CarB2 ( 130, 691 ) (Angle.degrees 180)

        otherCarWithRoute =
            case otherCarDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx otherCar

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            [ otherCarWithRoute
            ]

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
                |> World.setCar otherCarWithRoute.id otherCarWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


noCollisionSetupDifferentLanes : Round
noCollisionSetupDifferentLanes =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 60, 745 ) (Angle.degrees 0)

        otherCar =
            buildCar CarB2 ( 100, 774 ) (Angle.degrees 180)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


noCollisionSetupIntersection : Round
noCollisionSetupIntersection =
    let
        world =
            worldWithIntersection

        car =
            buildCar CarA1 ( 80, 666 ) (Angle.degrees 0)

        otherCar =
            buildCar CarB2 ( 133, 690 ) (Angle.degrees 90)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


redTrafficLightsSetup : Round
redTrafficLightsSetup =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork downIntersectionEntryNodePosition

        car =
            buildCar CarA1 ( 134, 600 ) (Angle.degrees 90)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        world =
            worldWithIntersection

        carDestination =
            RoadNetwork.findNodeByPosition world.roadNetwork leftIntersectionEntryNodePosition

        car =
            buildCar CarA1 ( 47, 665 ) (Angle.degrees 0)

        carWithRoute =
            case carDestination of
                Just nodeCtx ->
                    Car.createRoute nodeCtx car

                Nothing ->
                    Debug.todo "invalid test fixture"

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar carWithRoute.id carWithRoute
    in
    Round worldWithCars carWithRoute otherCars seed


yieldSetup : Bool -> Round
yieldSetup hasPriorityTraffic =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)

        otherCars =
            if hasPriorityTraffic then
                [ buildCar CarB2 ( 80, 720 ) (Angle.degrees 270)
                ]

            else
                []
    in
    Round world car otherCars seed


yieldWithPriorityTrafficSetup : Round
yieldWithPriorityTrafficSetup =
    yieldSetup True


yieldWithoutPriorityTrafficSetup : Round
yieldWithoutPriorityTrafficSetup =
    yieldSetup False


stopSetup : Round
stopSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 3, 1 )
                |> World.buildRoadAt ( 3, 2 )
                |> World.buildRoadAt ( 3, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)
                |> Car.move

        otherCars =
            []
    in
    Round world car otherCars seed


yieldAfterStopSetup : Round
yieldAfterStopSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 2 )
                |> World.buildRoadAt ( 2, 1 )
                |> World.buildRoadAt ( 2, 2 )
                |> World.buildRoadAt ( 2, 3 )

        car =
            buildCar CarA1 ( 0, 640 ) (Angle.degrees 0)
                |> Car.stopAtIntersection

        otherCars =
            [ buildCar CarB2 ( 80, 720 ) (Angle.degrees 270)
            ]
    in
    Round world car otherCars seed


largeWorldSetup : Int -> Round
largeWorldSetup carsAmount =
    let
        world =
            largeWorld

        worldWithCars =
            spawnCars carsAmount world (Random.initialSeed 224)
    in
    case Dict.values worldWithCars.cars of
        x :: xs ->
            Round worldWithCars x xs seed

        [] ->
            Debug.todo "no cars"



--
-- Utility
--


type TestCar
    = CarA1
    | CarB2


buildCar : TestCar -> ( Float, Float ) -> Angle -> Car
buildCar option ( x, y ) rotation =
    let
        ( kind, id ) =
            case option of
                CarA1 ->
                    ( Car.SedanA, 1 )

                CarB2 ->
                    ( Car.SedanB, 2 )
    in
    Car.new kind
        |> Car.withPosition (toLMPoint2d x y)
        |> Car.withRotation rotation
        |> Car.withVelocity Car.maxVelocity
        |> Car.build id
        |> Car.startMoving


spawnCars : Int -> World -> Random.Seed -> World
spawnCars n world aSeed =
    if n == 0 then
        world

    else
        let
            ( nextWorld, nextSeed, _ ) =
                World.spawnCar aSeed world
        in
        spawnCars (n - 1) nextWorld nextSeed
