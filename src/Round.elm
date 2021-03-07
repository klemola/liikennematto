module Round exposing
    ( Round
    , Rule(..)
    , checkCollisionRules
    , checkIntersectionRules
    , play
    )

import Car exposing (Car, Status(..))
import Circle2d
import Config exposing (carFieldOfView, carLength, carRotationTolerance, tileSize)
import Direction2d
import Geometry exposing (LMTriangle2d, toLMUnits)
import Maybe.Extra
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


play : Round -> RoundResults
play round =
    if Car.isConfused round.activeCar then
        toResults round

    else
        round
            |> checkRules
            |> updateCar
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
            if Car.isStopping round.activeCar then
                applyCarAction Car.startMoving round

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
            applyCarAction Car.waitForTrafficLights round

        YieldAtIntersection ->
            applyCarAction Car.yield round

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                round

            else
                applyCarAction Car.stopAtIntersection round


applyCarAction : (Car -> Car) -> Round -> Round
applyCarAction action round =
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
                round
                    |> removeActiveCarPriority
                    |> chooseNextConnection
                    |> applyCarAction Car.move

            else
                applyCarAction Car.move round

        ParkedAtLot ->
            applyCarAction Car.beginLeaveLot round

        _ ->
            round


removeActiveCarPriority : Round -> Round
removeActiveCarPriority round =
    { round | carsWithPriority = Set.remove round.activeCar.id round.carsWithPriority }


chooseNextConnection : Round -> Round
chooseNextConnection round =
    case round.activeCar.route of
        nodeCtx :: _ ->
            chooseRandomRoute round nodeCtx

        _ ->
            applyCarAction Car.markAsConfused round


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



--
-- Rules
--


carProximityCutoff =
    toLMUnits tileSize


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar, carsWithPriority } =
    Maybe.Extra.orListLazy
        [ \() -> checkNearCollision activeCar otherCars
        , \() ->
            if Set.member activeCar.id carsWithPriority then
                Nothing

            else
                checkPathCollision activeCar otherCars
        ]


checkNearCollision : Car -> List Car -> Maybe Rule
checkNearCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        forwardShiftedCarPosition =
            activeCar.position
                |> Geometry.translatePointIn carDirection (toLMUnits <| carLength / 2)

        -- A circle that covers the front half of the car, and the area right before the car
        checkArea =
            Geometry.circleAt forwardShiftedCarPosition (carLength / 2)
    in
    collisionWith otherCars
        (\otherCar -> Circle2d.intersectsBoundingBox (Car.boundingBox otherCar) checkArea)
        |> Maybe.map PreventCollision


checkPathCollision : Car -> List Car -> Maybe Rule
checkPathCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        carSightTriangle =
            Geometry.fieldOfViewTriangle activeCar.position carDirection carFieldOfView carProximityCutoff
    in
    collisionWith otherCars (pathsCouldCollideWith carSightTriangle activeCar)
        |> Maybe.map AvoidCollision


pathsCouldCollideWith : LMTriangle2d -> Car -> Car -> Bool
pathsCouldCollideWith fieldOfViewTriangle activeCar otherCar =
    let
        checkRequired =
            Triangle2d.contains otherCar.position fieldOfViewTriangle

        headingRoughlyInTheSameDirection =
            Quantity.equalWithin carRotationTolerance activeCar.rotation otherCar.rotation
    in
    checkRequired
        && not headingRoughlyInTheSameDirection
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
