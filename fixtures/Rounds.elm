module Rounds exposing
    ( collisionSetupCollided
    , collisionSetupNearCollision
    , collisionSetupPathsIntersect
    , connectedRoadsSetup
    , greenTrafficLightsSetup
    , largeWorldSetup
    , noCollisionSetupDifferentLanes
    , noCollisionSetupIntersection
    , redTrafficLightsSetup
    , yieldSlowDownSetup
    , yieldWithPriorityTrafficSetup1
    , yieldWithPriorityTrafficSetup2
    , yieldWithoutPriorityTrafficSetup
    )

import Angle exposing (Angle)
import Car exposing (Car)
import Dict
import Geometry exposing (LMPoint2d)
import Quantity
import Random
import RoadNetwork
import Round exposing (Round)
import Speed exposing (Speed)
import Steering
import Utility exposing (toLMPoint2d)
import World exposing (World)
import Worlds exposing (largeWorld, worldWithFourWayIntersection, worldWithThreeWayIntersection)


seed : Random.Seed
seed =
    Random.initialSeed 42


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 0, 720 ) (Angle.degrees 0) Steering.maxVelocity

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
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 102, 668 ) (Angle.degrees 30) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 134 720)

        otherCar =
            buildCar CarB2 ( 160, 691 ) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 80 694)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


collisionSetupNearCollision : Round
collisionSetupNearCollision =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 110, 670 ) (Angle.degrees 45) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 134 720)

        otherCar =
            buildCar CarB2 ( 130, 691 ) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 80 694)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


collisionSetupCollided : Round
collisionSetupCollided =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 134, 674 ) (Angle.degrees 90) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 134 720)

        otherCar =
            buildCar CarB2 ( 130, 691 ) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 80 694)

        otherCars =
            [ otherCar
            ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


noCollisionSetupDifferentLanes : Round
noCollisionSetupDifferentLanes =
    let
        world =
            World.empty
                |> World.buildRoadAt ( 1, 1 )
                |> World.buildRoadAt ( 2, 1 )

        car =
            buildCar CarA1 ( 60, 745 ) (Angle.degrees 0) Steering.maxVelocity

        otherCar =
            buildCar CarB2 ( 100, 774 ) (Angle.degrees 180) Steering.maxVelocity

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
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 80, 666 ) (Angle.degrees 0) Steering.maxVelocity

        otherCar =
            buildCar CarB2 ( 133, 690 ) (Angle.degrees 90) Steering.maxVelocity

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
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 134, 600 ) (Angle.degrees 90) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 134 640)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 ( 47, 665 ) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (toLMPoint2d 80 666)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


yieldWithPriorityTrafficSetup1 : Round
yieldWithPriorityTrafficSetup1 =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 ( 140, 586 ) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (toLMPoint2d 160 586)

        otherCar =
            buildCar CarB2 ( 213, 542 ) (Angle.degrees 90) Steering.maxVelocity

        otherCars =
            [ otherCar ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


yieldWithPriorityTrafficSetup2 : Round
yieldWithPriorityTrafficSetup2 =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 ( 140, 586 ) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (toLMPoint2d 160 586)

        otherCar =
            buildCar CarB2 ( 186, 642 ) (Angle.degrees 270) Steering.maxVelocity

        otherCars =
            [ otherCar ]

        worldWithCars =
            world
                |> World.setCar car.id car
                |> World.setCar otherCar.id otherCar
    in
    Round worldWithCars car otherCars seed


yieldWithoutPriorityTrafficSetup : Round
yieldWithoutPriorityTrafficSetup =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 ( 140, 585 ) (Angle.degrees 0) Quantity.zero

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car [] seed


yieldSlowDownSetup : Round
yieldSlowDownSetup =
    let
        world =
            worldWithThreeWayIntersection

        car =
            buildCar CarA1 ( 120, 586 ) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (toLMPoint2d 160 586)

        otherCars =
            []

        worldWithCars =
            world
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


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
            -- Dummy value, should never get here
            connectedRoadsSetup



--
-- Utility
--


type TestCar
    = CarA1
    | CarB2


buildCar : TestCar -> ( Float, Float ) -> Angle -> Speed -> Car
buildCar option ( x, y ) orientation velocity =
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
        |> Car.withOrientation orientation
        |> Car.withVelocity velocity
        |> Car.build id Nothing
        |> Car.startMoving


routeCarByDestination : World -> LMPoint2d -> Car -> Car
routeCarByDestination world position car =
    let
        destination =
            RoadNetwork.findNodeByPosition world.roadNetwork position
    in
    case destination of
        Just nodeCtx ->
            Car.createRoute nodeCtx car

        Nothing ->
            car


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
