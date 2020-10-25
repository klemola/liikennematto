module Simulation exposing (Model, Msg(..), initialModel, subscriptions, update)

import Board exposing (Board)
import Car
import Cell exposing (Cell)
import Config exposing (tileSize)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (BuildingKind, Lot, NewLot)
import Random
import Random.List
import Round
import SharedState
    exposing
        ( Cars
        , Lots
        , SharedState
        , SharedStateUpdate
        , SimulationState(..)
        )
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..))
import Time
import TrafficLight


type alias Model =
    { coinTossResult : Bool
    , randomDirections : List Direction
    }


type alias ShuffledBoard =
    List ( Cell, Tile )


type Msg
    = UpdateTraffic Time.Posix
    | UpdateEnvironment Time.Posix
    | PrepareGeneration Time.Posix
    | GenerateEnvironment ShuffledBoard
    | ReceiveCoinTossResult Bool
    | ReceiveRandomDirections (List Direction)


initialModel : Model
initialModel =
    { coinTossResult = False
    , randomDirections = Direction.all
    }


subscriptions : SharedState -> Sub Msg
subscriptions sharedState =
    case sharedState.simulationState of
        Simulation speed ->
            let
                ( slowTickSpeed, fastTickSpeed ) =
                    SharedState.simulationSpeedValues speed
            in
            Sub.batch
                [ Time.every slowTickSpeed UpdateEnvironment
                , Time.every fastTickSpeed UpdateTraffic
                , Time.every slowTickSpeed PrepareGeneration
                ]

        Paused ->
            Sub.none



--
-- Core update function and effects
--


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UpdateTraffic _ ->
            ( model
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            , updateTraffic model sharedState.board sharedState.cars
                |> SharedState.UpdateCars
            )

        UpdateEnvironment _ ->
            ( model
            , Cmd.none
            , updateEnvironment sharedState.board
                |> SharedState.UpdateBoard
            )

        PrepareGeneration _ ->
            ( model
            , shuffleBoardAndGenerateEnvironment sharedState.board
            , SharedState.NoUpdate
            )

        GenerateEnvironment shuffledBoard ->
            let
                { board, cars, lots } =
                    sharedState

                -- skip the generation if nothing unique can be generated, or if the road network is too small
                ssUpdate =
                    if Dict.size lots == List.length Lot.all || Dict.size board < 5 then
                        SharedState.NoUpdate

                    else
                        generateEnvironment sharedState shuffledBoard
                            |> SharedState.UpdateLots
            in
            ( model
            , Cmd.none
            , ssUpdate
            )

        ReceiveCoinTossResult result ->
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
        |> Random.generate ReceiveCoinTossResult


shuffleBoardAndGenerateEnvironment : Board -> Cmd Msg
shuffleBoardAndGenerateEnvironment board =
    Dict.toList board
        |> Random.List.shuffle
        |> Random.generate GenerateEnvironment



--
-- Environment logic
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


generateEnvironment : SharedState -> ShuffledBoard -> ( Lots, Cars )
generateEnvironment sharedState shuffledBoard =
    let
        { lots, cars } =
            sharedState

        existingBuildingKinds =
            lots
                |> Dict.map (\_ lot -> lot.content.kind)
                |> Dict.values

        -- Room for improvement: lots should be shuffled so that they don't have to be built in certain order
        nextUnusedLot =
            Lot.all
                |> List.filter
                    (\{ content } ->
                        not (List.member content.kind existingBuildingKinds)
                    )
                |> List.head

        roadOrientation newLot =
            newLot.content.entryDirection
                |> Direction.toOrientation
                |> Direction.oppositeOrientation
    in
    case nextUnusedLot of
        Just newLot ->
            findLotAnchor
                { targetOrientation = roadOrientation newLot
                , targetDirection = Direction.opposite newLot.content.entryDirection
                , newLot = newLot
                , sharedState = sharedState
                , shuffledBoard = shuffledBoard
                }
                |> Maybe.map (Lot.anchorTo newLot >> addLot lots cars)
                |> Maybe.withDefault ( lots, cars )

        Nothing ->
            ( lots, cars )


findLotAnchor :
    { targetOrientation : Orientation
    , targetDirection : Direction
    , newLot : NewLot
    , sharedState : SharedState
    , shuffledBoard : ShuffledBoard
    }
    -> Maybe ( Cell, Tile )
findLotAnchor { targetOrientation, targetDirection, newLot, sharedState, shuffledBoard } =
    let
        isCompatible ( cell, tile ) =
            case tile of
                TwoLaneRoad (Regular orientation) _ ->
                    (orientation == targetOrientation)
                        && SharedState.isEmptyArea
                            { origin =
                                Cell.next cell targetDirection
                                    |> Cell.bottomLeftCorner
                                    |> Lot.adjustOriginByAnchor newLot
                            , width = newLot.width
                            , height = newLot.height
                            }
                            sharedState

                _ ->
                    False
    in
    shuffledBoard
        |> List.filter isCompatible
        |> List.head


addLot : Lots -> Cars -> Lot -> ( Lots, Cars )
addLot lots cars lot =
    let
        nextLotId =
            SharedState.nextId lots

        nextCarId =
            SharedState.nextId cars

        newLots =
            Dict.insert nextLotId lot lots

        newCars =
            case Lot.resident lot of
                Just carKind ->
                    Dict.insert nextCarId
                        { kind = carKind
                        , position = Cell.bottomLeftCorner (Lot.entryCell lot)
                        , direction = Direction.next lot.content.entryDirection
                        , homeLotId = Just nextLotId
                        , status = Car.ParkedAtLot
                        }
                        cars

                _ ->
                    cars
    in
    ( newLots, newCars )



--
-- Traffic logic (cars)
--


updateTraffic : Model -> Board -> Cars -> Cars
updateTraffic model board cars =
    updateTrafficHelper
        { updateQueue = Dict.keys cars
        , cars = cars
        , model = model
        , board = board
        }


updateTrafficHelper :
    { updateQueue : List Int
    , cars : Cars
    , model : Model
    , board : Board
    }
    -> Cars
updateTrafficHelper { updateQueue, cars, model, board } =
    case updateQueue of
        activeCarId :: queue ->
            let
                nextRound updatedCar =
                    updateTrafficHelper
                        { updateQueue = queue
                        , cars = Dict.insert activeCarId updatedCar cars
                        , model = model
                        , board = board
                        }

                -- Room for improvement: only query cars that are nearby
                otherCars =
                    cars
                        |> Dict.filter (\k _ -> k /= activeCarId)
                        |> Dict.values
            in
            case Dict.get activeCarId cars of
                Just activeCar ->
                    Round.new board model.coinTossResult model.randomDirections activeCar otherCars
                        |> Round.attemptRespawn
                        |> Round.play
                        |> nextRound

                -- this should never happen, but the typesystem doesn't know that
                Nothing ->
                    cars

        [] ->
            cars
