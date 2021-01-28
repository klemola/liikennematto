module Round exposing
    ( Round
    , Rule(..)
    , checkCollisionRules
    , checkIntersectionRules
    , new
    , play
    )

import Car exposing (Car, Status(..))
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
        activeRulesByPriority round
            |> List.head
            |> Maybe.map (\rule -> ( applyRule round rule, round.seed ))
            |> Maybe.withDefault (updateCar round)


updateCar : Round -> ( Car, Random.Seed )
updateCar round =
    let
        { activeCar, seed } =
            round
    in
    case activeCar.status of
        Moving ->
            if Car.isAtTheEndOfLocalPath activeCar then
                chooseNextConnection round

            else
                ( Car.move activeCar, seed )

        ParkedAtLot ->
            ( Car.beginLeaveLot activeCar
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


applyRule : Round -> Rule -> Car
applyRule { activeCar } rule =
    case rule of
        AvoidCollision ->
            Car.skipRound activeCar

        WaitForTrafficLights ->
            Car.waitForTrafficLights activeCar

        YieldAtIntersection ->
            Car.yield activeCar

        StopAtIntersection ->
            if Car.isStoppedOrWaiting activeCar then
                activeCar

            else
                Car.stopAtIntersection activeCar


activeRulesByPriority : Round -> List Rule
activeRulesByPriority round =
    [ checkCollisionRules round
    , checkIntersectionRules round
    ]
        -- remove inactive rules
        |> List.filterMap identity


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar } =
    let
        -- TODO use a bounding box with padding -> larger than the car
        willCollideWithAnother =
            False
    in
    if willCollideWithAnother then
        Just AvoidCollision

    else
        Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { otherCars, activeCar, world } =
    let
        nextTile =
            Nothing

        priorityTraffic =
            []

        hasPriority =
            False

        shouldYield =
            not hasPriority && List.length priorityTraffic > 0

        shouldStop =
            not hasPriority && not (Car.isStoppedOrWaiting activeCar)
    in
    case nextTile of
        Just (Intersection (Signal trafficLights) _) ->
            --- TODO: dummy
            Just WaitForTrafficLights

        Just (Intersection (Yield _) _) ->
            if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        -- stop sign doubles as a yield sign
        Just (Intersection (Stop _) _) ->
            if shouldStop then
                Just StopAtIntersection

            else if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        _ ->
            Nothing
