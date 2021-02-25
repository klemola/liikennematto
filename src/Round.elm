module Round exposing
    ( Round
    , Rule(..)
    , checkCollisionRules
    , checkIntersectionRules
    , play
    )

import Angle
import Car exposing (Car, Status(..))
import Circle2d
import Config exposing (carLength, tileSize)
import Direction2d
import Geometry exposing (LMTriangle2d, toLMUnits)
import Maybe.Extra
import Point2d
import Quantity
import Random
import Random.List
import RoadNetwork
import Set exposing (Set)
import Tile
    exposing
        ( IntersectionControl(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import Triangle2d
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , seed : Random.Seed
    , carsWithPriority : Set Int
    }


type alias RoundResults =
    { car : Car
    , seed : Random.Seed
    , carsWithPriority : Set Int
    }


type Rule
    = AvoidCollision Int
    | PreventCollision Int
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


carProximityCutoff =
    toLMUnits tileSize


collisionCheckDistance =
    toLMUnits 20


carRotationTolerance =
    Angle.degrees 5


fieldOfView =
    Angle.degrees 70


play : Round -> RoundResults
play round =
    if Car.isConfused round.activeCar then
        toResults round

    else
        round
            |> checkRules
            |> updateCar
            |> checkPriority
            |> toResults


toResults : Round -> RoundResults
toResults round =
    { car = round.activeCar
    , seed = round.seed
    , carsWithPriority = round.carsWithPriority
    }


checkRules : Round -> Round
checkRules round =
    case ruleToApply round of
        Just rule ->
            applyRule round rule

        Nothing ->
            if round.activeCar.status == Stopping then
                applyCarAction round Car.startMoving

            else
                round


ruleToApply : Round -> Maybe Rule
ruleToApply round =
    Maybe.Extra.orListLazy
        [ \() -> checkCollisionRules round
        , \() -> checkIntersectionRules round
        ]


applyRule : Round -> Rule -> Round
applyRule round rule =
    let
        { activeCar, carsWithPriority } =
            round

        updatePriority carId =
            carsWithPriority
                |> Set.insert carId
    in
    case rule of
        AvoidCollision otherCarId ->
            { round
                | activeCar = Car.giveWay activeCar
                , carsWithPriority = updatePriority otherCarId
            }

        PreventCollision otherCarId ->
            { round
                | activeCar = Car.break activeCar
                , carsWithPriority = updatePriority otherCarId
            }

        WaitForTrafficLights ->
            applyCarAction round Car.waitForTrafficLights

        YieldAtIntersection ->
            applyCarAction round Car.yield

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction round Car.stopAtIntersection


applyCarAction : Round -> (Car -> Car) -> Round
applyCarAction round action =
    { round | activeCar = action round.activeCar }


updateCar : Round -> Round
updateCar round =
    let
        { activeCar } =
            round
    in
    case activeCar.status of
        Moving ->
            if Car.isAtTheEndOfLocalPath activeCar then
                let
                    nextRound =
                        chooseNextConnection round
                in
                applyCarAction nextRound Car.move

            else
                applyCarAction round Car.move

        ParkedAtLot ->
            applyCarAction round Car.beginLeaveLot

        _ ->
            round


chooseNextConnection : Round -> Round
chooseNextConnection round =
    case round.activeCar.route of
        nodeCtx :: _ ->
            chooseRandomRoute round nodeCtx

        _ ->
            applyCarAction round Car.markAsConfused


chooseRandomRoute : Round -> RoadNetwork.RNNodeContext -> Round
chooseRandomRoute round nodeCtx =
    let
        { activeCar, world, seed } =
            round

        randomConnectionGenerator =
            RoadNetwork.getOutgoingConnections nodeCtx
                |> Random.List.choose
                |> Random.map Tuple.first

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed

        nextCar =
            connection
                |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
                |> Maybe.map (Car.buildRoute activeCar)
                |> Maybe.withDefault activeCar
    in
    { round | activeCar = nextCar, seed = nextSeed }


checkPriority : Round -> Round
checkPriority round =
    let
        activeCarHasPriority =
            Set.member round.activeCar.id round.carsWithPriority

        shouldClearPriority () =
            List.all (atSafeDistanceFrom round.activeCar) round.otherCars
    in
    if activeCarHasPriority && shouldClearPriority () then
        { round | carsWithPriority = Set.remove round.activeCar.id round.carsWithPriority }

    else
        round


atSafeDistanceFrom : Car -> Car -> Bool
atSafeDistanceFrom car1 car2 =
    Point2d.distanceFrom car1.position car2.position
        |> Quantity.greaterThanOrEqualTo carProximityCutoff



--
-- Rules
--


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar, carsWithPriority } =
    if Set.member activeCar.id carsWithPriority then
        Nothing

    else
        Maybe.Extra.orListLazy
            [ \() -> checkNearCollision activeCar otherCars
            , \() -> checkPathCollision activeCar otherCars
            ]


checkNearCollision : Car -> List Car -> Maybe Rule
checkNearCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        forwardShiftedCarPosition =
            activeCar.position
                |> Geometry.translatePointIn carDirection collisionCheckDistance
    in
    collisionWith otherCars
        (\otherCar ->
            Geometry.circleAt otherCar.position (carLength / 2)
                |> Circle2d.contains forwardShiftedCarPosition
        )
        |> Maybe.map PreventCollision


checkPathCollision : Car -> List Car -> Maybe Rule
checkPathCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        carSightTriangle =
            Geometry.fieldOfViewTriangle activeCar.position carDirection fieldOfView carProximityCutoff
    in
    collisionWith otherCars (pathsCouldCollideWith carSightTriangle activeCar)
        |> Maybe.map PreventCollision


pathsCouldCollideWith : LMTriangle2d -> Car -> Car -> Bool
pathsCouldCollideWith fieldOfViewTriangle activeCar otherCar =
    let
        checkRequired =
            Triangle2d.contains otherCar.position fieldOfViewTriangle

        roughlyGoingTheSameWay =
            Quantity.equalWithin carRotationTolerance activeCar.rotation otherCar.rotation
    in
    checkRequired
        && not roughlyGoingTheSameWay
        && Geometry.pathsCouldCollide activeCar.localPath otherCar.localPath


collisionWith : List Car -> (Car -> Bool) -> Maybe Int
collisionWith carsToCheck collisionPredicate =
    case carsToCheck of
        next :: others ->
            if collisionPredicate next then
                Just next.id

            else
                collisionWith others collisionPredicate

        [] ->
            Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { otherCars, activeCar, world } =
    Nothing
