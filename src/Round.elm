module Round exposing
    ( Round
    , Rule(..)
    , checkCollisionRules
    , checkIntersectionRules
    , new
    , play
    )

import Angle
import BoundingBox2d
import Car exposing (Car, Status(..))
import Config exposing (tileSize)
import Direction2d
import Geometry
import Maybe.Extra
import Point2d exposing (toTuple)
import Quantity
import Random
import Random.List
import RoadNetwork
import Tile
    exposing
        ( IntersectionControl(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , seed : Random.Seed
    }


type Rule
    = AvoidCollision
    | PreventCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : World -> Random.Seed -> Car -> List Car -> Round
new world seed activeCar otherCars =
    { world = world
    , activeCar = activeCar
    , otherCars = otherCars
    , seed = seed
    }


play : Round -> ( Car, Random.Seed )
play round =
    if Car.isConfused round.activeCar then
        ( round.activeCar, round.seed )

    else
        let
            rule =
                ruleToApply round
        in
        rule
            |> Maybe.map (applyRule round)
            |> Maybe.withDefault round.activeCar
            |> (\car -> updateCar rule { round | activeCar = car })


ruleToApply : Round -> Maybe Rule
ruleToApply round =
    Maybe.Extra.orListLazy
        [ \() -> checkCollisionRules round
        , \() -> checkIntersectionRules round
        ]


applyRule : Round -> Rule -> Car
applyRule { activeCar } rule =
    case rule of
        AvoidCollision ->
            Car.giveWay activeCar

        PreventCollision ->
            Car.break activeCar

        WaitForTrafficLights ->
            Car.waitForTrafficLights activeCar

        YieldAtIntersection ->
            Car.yield activeCar

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                activeCar

            else
                Car.stopAtIntersection activeCar


updateCar : Maybe Rule -> Round -> ( Car, Random.Seed )
updateCar activeRule round =
    let
        { activeCar, seed } =
            round
    in
    case activeCar.status of
        Moving ->
            if Car.isAtTheEndOfLocalPath activeCar then
                let
                    ( nextCar, nextSeed ) =
                        chooseNextConnection round
                in
                ( nextCar
                    |> Car.move
                , nextSeed
                )

            else
                ( Car.move activeCar, seed )

        ParkedAtLot ->
            ( Car.beginLeaveLot activeCar
            , seed
            )

        Stopping ->
            let
                base =
                    if Maybe.Extra.isNothing activeRule then
                        Car.startMoving activeCar

                    else
                        activeCar
            in
            ( Car.move base
            , seed
            )

        _ ->
            ( activeCar, seed )


chooseNextConnection : Round -> ( Car, Random.Seed )
chooseNextConnection round =
    case round.activeCar.route of
        nodeCtx :: _ ->
            chooseRandomRoute round nodeCtx

        _ ->
            ( Car.markAsConfused round.activeCar, round.seed )


chooseRandomRoute : Round -> RoadNetwork.RNNodeContext -> ( Car, Random.Seed )
chooseRandomRoute { activeCar, seed, world } nodeCtx =
    let
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
    ( nextCar, nextSeed )


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar } =
    Maybe.Extra.orListLazy
        [ \() -> checkNearCollision activeCar otherCars
        , \() -> checkPathCollision activeCar otherCars
        ]


collisionCheckDistance =
    Geometry.toLMUnits 20


checkNearCollision : Car -> List Car -> Maybe Rule
checkNearCollision activeCar otherCars =
    let
        carDirection =
            Direction2d.fromAngle activeCar.rotation

        shiftedActiveCarBB =
            Car.boundingBox activeCar
                |> BoundingBox2d.translateIn carDirection collisionCheckDistance

        collisionWith otherCar =
            BoundingBox2d.intersects shiftedActiveCarBB (Car.boundingBox otherCar)
    in
    if List.any collisionWith otherCars then
        Just PreventCollision

    else
        Nothing


farCollisionCheckDistance =
    Geometry.toLMUnits tileSize


carRotationTolerance =
    Angle.degrees 5


checkPathCollision : Car -> List Car -> Maybe Rule
checkPathCollision activeCar otherCars =
    if List.any (pathsCouldCollideWith activeCar) otherCars then
        Just AvoidCollision

    else
        Nothing


pathsCouldCollideWith : Car -> Car -> Bool
pathsCouldCollideWith activeCar otherCar =
    let
        distanceBetweenCars =
            Point2d.distanceFrom activeCar.position otherCar.position

        roughlyGoingTheSameWay =
            Quantity.equalWithin carRotationTolerance activeCar.rotation otherCar.rotation
    in
    not roughlyGoingTheSameWay
        && Quantity.lessThan farCollisionCheckDistance distanceBetweenCars
        && Geometry.pathsCouldCollide activeCar.localPath otherCar.localPath


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { otherCars, activeCar, world } =
    Nothing
