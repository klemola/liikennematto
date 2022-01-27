module Data.Rounds exposing
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
import Data.Worlds
    exposing
        ( largeWorld
        , simpleWorld
        , worldWithFourWayIntersection
        , worldWithThreeWayIntersection
        )
import Dict
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.RoadNetwork as RoadNetwork
import Model.World as World exposing (World)
import Point2d
import Quantity
import Random
import Simulation.Pathfinding as Pathfinding
import Simulation.Round exposing (Round)
import Simulation.Steering as Steering
import Simulation.Traffic as Traffic
import Speed exposing (Speed)


seed : Random.Seed
seed =
    Random.initialSeed 42


connectedRoadsSetup : Round
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
                |> World.setCar car.id car
    in
    Round worldWithCars car otherCars seed


collisionSetupPathsIntersect : Round
collisionSetupPathsIntersect =
    let
        world =
            worldWithFourWayIntersection

        car =
            buildCar CarA1 (Point2d.meters 20.4 133.4) (Angle.degrees 30) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 26 144)

        otherCar =
            buildCar CarB2 (Point2d.meters 32 138) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 16 138)

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
            buildCar CarA1 (Point2d.meters 22 133.8) (Angle.degrees 45) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 26 144)

        otherCar =
            buildCar CarB2 (Point2d.meters 26 138) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 16 138)

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
            buildCar CarA1 (Point2d.meters 26 134.6) (Angle.degrees 90) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 26 144)

        otherCar =
            buildCar CarB2 (Point2d.meters 25.2 138) (Angle.degrees 180) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 16 138)

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
            buildCar CarA1 (Point2d.meters 16 134) (Angle.degrees 0) Steering.maxVelocity

        otherCar =
            buildCar CarB2 (Point2d.meters 26 138) (Angle.degrees 90) Steering.maxVelocity

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
            buildCar CarA1 (Point2d.meters 26 120) (Angle.degrees 90) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 26 128)

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
            buildCar CarA1 (Point2d.meters 9.4 134) (Angle.degrees 0) Steering.maxVelocity
                |> routeCarByDestination world (Point2d.meters 16 134)

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
            buildCar CarA1 (Point2d.meters 28 118) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (Point2d.meters 32 118)

        otherCar =
            buildCar CarB2 (Point2d.meters 42 108.4) (Angle.degrees 90) Steering.maxVelocity

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
            buildCar CarA1 (Point2d.meters 28 118) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (Point2d.meters 32 118)

        otherCar =
            buildCar CarB2 (Point2d.meters 38 128.4) (Angle.degrees 270) Steering.maxVelocity

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
            buildCar CarA1 (Point2d.meters 28 118) (Angle.degrees 0) Quantity.zero

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
            buildCar CarA1 (Point2d.meters 24 118) (Angle.degrees 0) Quantity.zero
                |> routeCarByDestination world (Point2d.meters 32 118)

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


buildCar : TestCar -> LMPoint2d -> Angle -> Speed -> Car
buildCar option position orientation velocity =
    let
        ( kind, id ) =
            case option of
                CarA1 ->
                    ( Car.TestCar, 1 )

                CarB2 ->
                    ( Car.TestCar, 2 )
    in
    Car.new kind
        |> Car.withPosition position
        |> Car.withOrientation orientation
        |> Car.withVelocity velocity
        |> Car.build id
        |> Steering.startMoving


routeCarByDestination : World -> LMPoint2d -> Car -> Car
routeCarByDestination world position car =
    let
        destination =
            RoadNetwork.findNodeByPosition world.roadNetwork position
    in
    case destination of
        Just nodeCtx ->
            Pathfinding.createRoute nodeCtx car

        Nothing ->
            car


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
