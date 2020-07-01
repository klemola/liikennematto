module Round exposing
    ( Round
    , Rule(..)
    , applyRule
    , attemptRespawn
    , checkCollisionRules
    , checkIntersectionRules
    , checkMovementRules
    , checkTurningRules
    , new
    , play
    )

import Board exposing (Board)
import Car exposing (Car, Status(..))
import Coords exposing (Coords)
import Direction exposing (Direction)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
import TrafficLight


type alias Round =
    { board : Board
    , activeCar : Car
    , otherCars : List Car
    , currentTile : Tile
    , nextCoords : Coords
    , nextTile : Tile
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type Rule
    = MovementBlocked
    | TurningRequired
    | AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : Board -> Bool -> List Direction -> Car -> List Car -> Round
new board coinTossResult randomDirections activeCar otherCars =
    let
        nextCoords =
            Coords.next activeCar.coords activeCar.direction
    in
    { board = board
    , activeCar = activeCar
    , otherCars = otherCars
    , coinTossResult = coinTossResult
    , randomDirections = randomDirections
    , nextCoords = nextCoords
    , currentTile =
        Board.getSafe activeCar.coords board
    , nextTile =
        Board.getSafe nextCoords board
    }


attemptRespawn : Round -> Round
attemptRespawn round =
    let
        { otherCars, board, activeCar } =
            round

        isEmptyRoad coords =
            List.all (\oc -> oc.coords /= coords) otherCars

        spawn coords =
            { round | activeCar = Car.spawn coords activeCar }
    in
    if Car.isRespawning activeCar then
        Board.roadCoords board
            |> List.filter isEmptyRoad
            |> List.head
            |> Maybe.map spawn
            |> Maybe.withDefault round

    else
        round


play : Round -> Car
play round =
    activeRulesByPriority round
        |> List.head
        |> Maybe.map (applyRule round)
        |> Maybe.withDefault (Car.move round.activeCar)


applyRule : Round -> Rule -> Car
applyRule { activeCar, board, randomDirections } rule =
    case rule of
        MovementBlocked ->
            Car.skipRound activeCar

        TurningRequired ->
            let
                oppositeDirection =
                    Direction.opposite activeCar.direction

                isLeftOrRightTurn dir =
                    dir /= activeCar.direction && dir /= oppositeDirection

                seeRoadAhead dir =
                    case Board.getSafe (Coords.next activeCar.coords dir) board of
                        Terrain ->
                            False

                        _ ->
                            True

                direction =
                    randomDirections
                        |> List.filter isLeftOrRightTurn
                        |> List.filter seeRoadAhead
                        |> List.head
                        |> Maybe.withDefault oppositeDirection
            in
            Car.turn direction activeCar

        AvoidCollision ->
            Car.skipRound activeCar

        WaitForTrafficLights ->
            Car.waitForTrafficLights activeCar

        YieldAtIntersection ->
            Car.yield activeCar

        StopAtIntersection ->
            if Car.onTheMove activeCar then
                Car.stopAtIntersection activeCar

            else
                activeCar


activeRulesByPriority : Round -> List Rule
activeRulesByPriority round =
    [ checkCollisionRules round
    , checkTurningRules round
    , checkIntersectionRules round
    , checkMovementRules round
    ]
        -- remove inactive rules
        |> List.filterMap identity


checkMovementRules : Round -> Maybe Rule
checkMovementRules { currentTile, nextTile, activeCar } =
    let
        canMove =
            Tile.connected activeCar.direction currentTile nextTile
    in
    if canMove then
        Nothing

    else
        Just MovementBlocked


checkTurningRules : Round -> Maybe Rule
checkTurningRules { currentTile, nextTile, coinTossResult, activeCar } =
    let
        -- turn every now and then at an intersection
        -- cars in intersections can block the traffic, so this also works as a sort of a tie-breaker
        shouldTurnRandomly =
            coinTossResult && Tile.isIntersection currentTile && not (Car.isTurning activeCar)
    in
    -- Room for improvement: base turning rule on whether roads connect in car's direction
    -- TODO: turn if facing a one-way street of opposite direction
    if Tile.isTerrain nextTile || shouldTurnRandomly then
        Just TurningRequired

    else
        Nothing


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, nextCoords, nextTile, activeCar } =
    let
        oppositeDirection =
            Direction.opposite activeCar.direction

        willCollideWithAnother =
            case nextTile of
                -- car moving towards another in an opposite direction will not cause a collision
                TwoLaneRoad (Regular _) Both ->
                    List.any (\c -> c.coords == nextCoords && c.direction /= oppositeDirection) otherCars

                -- intersections, curves and deadends should be clear before entering (slightly naive logic)
                -- TODO: consider both lanes of one-way traffic
                _ ->
                    List.any (\c -> c.coords == nextCoords) otherCars
    in
    if willCollideWithAnother then
        Just AvoidCollision

    else
        Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { board, otherCars, nextTile, nextCoords, activeCar } =
    let
        priorityDirections =
            Tile.priorityDirections (Board.getSafe nextCoords board)

        priorityTraffic =
            priorityDirections
                -- get tile coordinates relative to the intersection at "nextCoords"
                |> List.map (Coords.next nextCoords)
                -- add the intersection
                |> List.append [ Coords.next activeCar.coords activeCar.direction ]
                |> List.concatMap (Coords.filterBy otherCars)

        hasPriority =
            List.member activeCar.direction priorityDirections

        shouldYield =
            not hasPriority && List.length priorityTraffic > 0

        shouldStop =
            not hasPriority && Car.onTheMove activeCar
    in
    case nextTile of
        Intersection (Signal trafficLights) _ ->
            if TrafficLight.trafficAllowedFromDirection trafficLights activeCar.direction then
                Nothing

            else
                Just WaitForTrafficLights

        Intersection (Yield _) _ ->
            if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        -- stop sign doubles as a yield sign
        Intersection (Stop _) _ ->
            if shouldStop then
                Just StopAtIntersection

            else if shouldYield then
                Just YieldAtIntersection

            else
                Nothing

        _ ->
            Nothing
