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

import Board exposing (Board)
import Car exposing (Car, Status(..))
import Cell
import Config exposing (tileSize)
import Direction exposing (Direction)
import Position exposing (Position)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
import TrafficLight


type alias Round =
    { board : Board
    , activeCar : Car
    , otherCars : List Car
    , currentTile : Maybe Tile
    , nextPosition : Position
    , nextTile : Maybe Tile
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type Rule
    = TurningRequired
    | AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


new : Board -> Bool -> List Direction -> Car -> List Car -> Round
new board coinTossResult randomDirections activeCar otherCars =
    let
        nextPosition =
            Position.shiftBy tileSize activeCar.position activeCar.direction
    in
    { board = board
    , activeCar = activeCar
    , otherCars = otherCars
    , coinTossResult = coinTossResult
    , randomDirections = randomDirections
    , nextPosition = nextPosition
    , currentTile =
        Board.get (activeCar.position |> Cell.fromPosition) board
    , nextTile =
        Board.get (nextPosition |> Cell.fromPosition) board
    }


attemptRespawn : Round -> Round
attemptRespawn round =
    let
        { otherCars, board, activeCar } =
            round

        isEmptyRoad cell =
            List.all (\oc -> oc.position /= Cell.bottomLeftCorner cell) otherCars

        spawn cell =
            { round | activeCar = Car.spawn (Cell.bottomLeftCorner cell) activeCar }
    in
    if Car.isRespawning activeCar then
        Board.roadPiecePositions board
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
applyRule { activeCar, board, currentTile, randomDirections } rule =
    case rule of
        TurningRequired ->
            let
                oppositeDirection =
                    Direction.opposite activeCar.direction

                isLeftOrRightTurn dir =
                    dir /= activeCar.direction && dir /= oppositeDirection

                seeRoadAhead dir =
                    case
                        ( currentTile
                        , Board.get
                            (Cell.next (activeCar.position |> Cell.fromPosition) dir)
                            board
                        )
                    of
                        ( Just current, Just other ) ->
                            Tile.connected dir current other

                        _ ->
                            False

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
checkTurningRules { board, currentTile, nextTile, coinTossResult, activeCar } =
    let
        tileRelativeToCarPosition dir =
            Board.get (Cell.next (activeCar.position |> Cell.fromPosition) dir) board

        leftAndRightTilesFromCarDirection =
            [ tileRelativeToCarPosition (Direction.previous activeCar.direction)
            , tileRelativeToCarPosition (Direction.next activeCar.direction)
            ]
                |> List.filterMap identity

        canContinue =
            case ( currentTile, nextTile ) of
                ( Just current, Just next ) ->
                    Tile.connected activeCar.direction current next

                _ ->
                    Car.isParkedAtLot activeCar

        canTurn =
            not (List.isEmpty leftAndRightTilesFromCarDirection)

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
        Just TurningRequired

    else
        Nothing


checkCollisionRules : Round -> Maybe Rule
checkCollisionRules { otherCars, nextPosition, nextTile, activeCar } =
    let
        oppositeDirection =
            Direction.opposite activeCar.direction

        willCollideWithAnother =
            case nextTile of
                -- car moving towards another in an opposite direction will not cause a collision
                Just (TwoLaneRoad (Regular _) Both) ->
                    List.any (\c -> c.position == nextPosition && c.direction /= oppositeDirection) otherCars

                -- curves are really just another "Regular" piece of road, but the coordinates are not precise
                -- enough to consider "lanes". Meanwhile we'll fall back on "Regular" road logic
                Just (TwoLaneRoad (Curve _) Both) ->
                    List.any (\c -> c.position == nextPosition && c.direction /= oppositeDirection) otherCars

                -- intersections and deadends should be clear before entering (slightly naive logic)
                Just _ ->
                    List.any (\c -> c.position == nextPosition) otherCars

                Nothing ->
                    False
    in
    if willCollideWithAnother then
        Just AvoidCollision

    else
        Nothing


checkIntersectionRules : Round -> Maybe Rule
checkIntersectionRules { otherCars, nextTile, nextPosition, activeCar } =
    let
        priorityDirections =
            case nextTile of
                Just tile ->
                    Tile.priorityDirections tile

                Nothing ->
                    []

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
