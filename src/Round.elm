module Round exposing
    ( Round
    , Rule(..)
    , attemptRespawn
    , checkCollisionRules
    , checkIntersectionRules
    , checkTurningRules
    , new
    , play
    )

import Car exposing (Car, Status(..))
import Cell
import Config exposing (tileSize)
import Direction exposing (Direction)
import Position exposing (Position)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
import TrafficLight
import World exposing (World)


type alias Round =
    { world : World
    , activeCar : Car
    , otherCars : List Car
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type Rule
    = TurningRequired (List Direction)
    | AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : World -> Bool -> List Direction -> Car -> List Car -> Round
new world coinTossResult randomDirections activeCar otherCars =
    let
        nextPosition =
            Position.shiftBy tileSize activeCar.position activeCar.direction
    in
    { world = world
    , activeCar = activeCar
    , otherCars = otherCars
    , coinTossResult = coinTossResult
    , randomDirections = randomDirections
    }


attemptRespawn : Round -> Round
attemptRespawn round =
    let
        { otherCars, world, activeCar } =
            round

        isEmptyRoad cell =
            List.all (\oc -> oc.position /= Cell.bottomLeftCorner cell) otherCars

        spawn cell =
            { round | activeCar = Car.spawn (Cell.bottomLeftCorner cell) activeCar }
    in
    if Car.isRespawning activeCar then
        world
            |> World.roadCells
            |> List.filter isEmptyRoad
            |> List.head
            |> Maybe.map spawn
            |> Maybe.withDefault round

    else
        round


play : Round -> Car
play round =
    if not (Car.isRespawning round.activeCar) then
        activeRulesByPriority round
            |> List.head
            |> Maybe.map (applyRule round)
            |> Maybe.withDefault (Car.move round.activeCar)

    else
        round.activeCar


applyRule : Round -> Rule -> Car
applyRule { activeCar, world, randomDirections } rule =
    case rule of
        TurningRequired validTurningDirections ->
            case List.head validTurningDirections of
                Just dir ->
                    Car.turn dir activeCar

                Nothing ->
                    Car.turn (Direction.opposite activeCar.direction) activeCar

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
    [ checkTurningRules round
    , checkCollisionRules round
    , checkIntersectionRules round
    ]
        -- remove inactive rules
        |> List.filterMap identity


checkTurningRules : Round -> Maybe Rule
checkTurningRules { world, coinTossResult, activeCar } =
    let
        currentCell =
            Cell.fromPosition activeCar.position

        currentTile =
            World.tileAt currentCell world

        validTurningDirections =
            []

        canContinue =
            World.hasConnectedRoad
                { currentCell = currentCell
                , direction = activeCar.direction
                , world = world
                }

        canTurn =
            not (List.isEmpty validTurningDirections)

        -- turn every now and then at an intersection
        -- cars in intersections can block the traffic, so this also works as a sort of a tie-breaker
        shouldTurnRandomly =
            case currentTile of
                Just (Intersection _ _) ->
                    coinTossResult
                        && canTurn
                        && not (Car.isTurning activeCar)

                _ ->
                    False
    in
    if not canContinue || shouldTurnRandomly then
        Just (TurningRequired validTurningDirections)

    else
        Nothing


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, activeCar } =
    let
        oppositeDirection =
            Direction.opposite activeCar.direction

        nextPosition =
            Position.shiftBy 1 activeCar.position activeCar.direction

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
            World.tileAt
                (activeCar.position
                    |> Cell.fromPosition
                    |> Cell.next activeCar.direction
                )
                world

        priorityDirections =
            case nextTile of
                Just tile ->
                    Tile.priorityDirections tile

                Nothing ->
                    []

        nextPosition =
            Position.shiftBy 1 activeCar.position activeCar.direction

        priorityTraffic =
            priorityDirections
                -- get tile coordinates relative to the intersection at "nextPosition"
                |> List.map (Position.shiftBy tileSize nextPosition)
                -- add the intersection
                |> List.append [ Position.shiftBy tileSize activeCar.position activeCar.direction ]
                |> List.concatMap (Position.filterBy otherCars)

        hasPriority =
            List.member activeCar.direction priorityDirections

        shouldYield =
            not hasPriority && List.length priorityTraffic > 0

        shouldStop =
            not hasPriority && not (Car.isStoppedOrWaiting activeCar)
    in
    case nextTile of
        Just (Intersection (Signal trafficLights) _) ->
            if TrafficLight.trafficAllowedFromDirection trafficLights activeCar.direction then
                Nothing

            else
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
