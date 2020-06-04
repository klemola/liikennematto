module Simulation exposing (Model, Msg(..), initialModel, update)

import Board exposing (Board)
import Car exposing (Car, CarKind(..), Status(..))
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..), Orientation(..))
import Random
import Random.List
import SharedState exposing (Cars, SharedState, SharedStateUpdate)
import Tile exposing (IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
import TrafficLight


type alias Model =
    { activeCarId : ActiveCarId
    , coinTossResult : Bool
    , randomDirections : List Direction
    }


type alias ActiveCarId =
    Int


type Msg
    = UpdateTraffic
    | UpdateEnvironment
    | CoinToss Bool
    | ReceiveRandomDirections (List Direction)


initialModel : Model
initialModel =
    { activeCarId = 1
    , coinTossResult = False
    , randomDirections = List.repeat 4 Right
    }



--
-- Core update function and effects
--


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UpdateTraffic ->
            let
                ( nextActiveCarId, nextCars ) =
                    updateTraffic model sharedState.board sharedState.cars
            in
            ( { model | activeCarId = nextActiveCarId }
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            , SharedState.UpdateCars nextCars
            )

        UpdateEnvironment ->
            ( model
            , Cmd.none
            , SharedState.UpdateBoard (updateEnvironment sharedState.board)
            )

        CoinToss result ->
            ( { model | coinTossResult = result }, Cmd.none, SharedState.NoUpdate )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, Cmd.none, SharedState.NoUpdate )


shuffleDirections : Cmd Msg
shuffleDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


tossACoin : Cmd Msg
tossACoin =
    Random.weighted ( 60, False ) [ ( 40, True ) ]
        |> Random.generate CoinToss



--
-- Environment logic (traffic lights)
--


updateEnvironment : Board -> Board
updateEnvironment board =
    -- Room for improvement: consider moving traffic light state from tiles to SharedState
    -- in order to make Tiles passive
    let
        updateTrafficLight tl =
            if tl.timeRemaining == 0 then
                TrafficLight.new (TrafficLight.advanceLight tl.kind) tl.facing

            else
                TrafficLight.advanceTimer tl

        updateTile tile =
            case tile of
                Intersection (Signal trafficLights) shape ->
                    let
                        next =
                            trafficLights
                                |> List.map updateTrafficLight
                                |> Signal
                    in
                    Intersection next shape

                _ ->
                    tile
    in
    Dict.map (\_ tile -> updateTile tile) board



--
-- Traffic logic (cars)
--


updateTraffic : Model -> Board -> Cars -> ( ActiveCarId, Cars )
updateTraffic model board cars =
    let
        saveChanges updatedCar =
            Dict.insert model.activeCarId updatedCar cars

        nextCars =
            case Dict.get model.activeCarId cars of
                Just activeCar ->
                    newRound model board cars activeCar
                        |> attemptRespawn
                        |> playRound
                        |> saveChanges

                Nothing ->
                    cars

        nextActiveCarId =
            if model.activeCarId == 0 || model.activeCarId == Dict.size cars then
                -- restart the cycle
                1

            else
                model.activeCarId + 1
    in
    ( nextActiveCarId, nextCars )



--
-- Round (individual car update)
--


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


newRound : Model -> Board -> Cars -> Car -> Round
newRound model board cars activeCar =
    let
        nextCoords =
            Coords.next activeCar.coords activeCar.direction
    in
    { board = board
    , activeCar = activeCar
    , otherCars =
        cars
            |> Dict.filter (\k _ -> k /= model.activeCarId)
            |> Dict.values
    , coinTossResult = model.coinTossResult
    , randomDirections = model.randomDirections
    , nextCoords = nextCoords
    , currentTile =
        Board.getSafe board activeCar.coords
    , nextTile =
        Board.getSafe board nextCoords
    }


playRound : Round -> Car
playRound round =
    let
        { activeCar } =
            round

        activeRulesByPriority =
            [ checkCollisionRules round
            , checkTurningRules round
            , checkIntersectionRules round
            , checkMovementRules round
            ]
                -- remove inactive rules
                |> List.filterMap identity

        car =
            List.head activeRulesByPriority
                |> Maybe.map (applyRule round)
                |> Maybe.withDefault (Car.move activeCar)
    in
    car


attemptRespawn : Round -> Round
attemptRespawn round =
    let
        { otherCars, board, activeCar } =
            round

        isEmptyRoad coords =
            List.all (\oc -> oc.coords /= coords) otherCars

        spawn coords =
            { round | activeCar = Car.spawn activeCar coords }
    in
    if Car.isRespawning activeCar then
        Board.roadCoords board
            |> List.filter isEmptyRoad
            |> List.head
            |> Maybe.map spawn
            |> Maybe.withDefault round

    else
        round


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
                    case Board.getSafe board (Coords.next activeCar.coords dir) of
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
            Car.turn activeCar direction

        AvoidCollision ->
            Car.skipRound activeCar

        WaitForTrafficLights ->
            Car.waitForTrafficLights activeCar

        YieldAtIntersection ->
            Car.yield activeCar

        StopAtIntersection ->
            case activeCar.status of
                StoppedAtIntersection 0 ->
                    activeCar

                StoppedAtIntersection n ->
                    Car.stopAtIntersection activeCar (n - 1)

                Moving ->
                    Car.stopAtIntersection activeCar 1

                _ ->
                    activeCar



--
-- Rules (behavior constraints)
--


type Rule
    = MovementBlocked
    | TurningRequired
    | AvoidCollision
    | WaitForTrafficLights
    | YieldAtIntersection
    | StopAtIntersection


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
                TwoLaneRoad (Regular _) ->
                    List.any (\c -> c.coords == nextCoords && c.direction /= oppositeDirection) otherCars

                -- intersections, curves and deadends should be clear before entering (slightly naive logic)
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
            Tile.priorityDirections (Board.getSafe board nextCoords)

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
            not hasPriority && Car.isMoving activeCar
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
