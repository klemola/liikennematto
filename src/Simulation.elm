module Simulation exposing (Model, Msg(..), initialModel, subscriptions, update)

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot, NewLot)
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
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
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
    | GenerateEnvironment ( ShuffledBoard, Maybe NewLot )
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
            let
                currentLotsAmount =
                    Dict.size sharedState.lots

                existingBuildingKinds =
                    sharedState.lots
                        |> Dict.map (\_ lot -> lot.content.kind)
                        |> Dict.values

                unusedLots =
                    List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) Lot.all
            in
            -- skip the generation if nothing unique can be generated, or if the road network is too small
            if List.isEmpty unusedLots || (Dict.size sharedState.board * 4) < currentLotsAmount then
                ( model, Cmd.none, SharedState.NoUpdate )

            else
                ( model
                , prepareRandomDataForGeneration sharedState.board unusedLots
                , SharedState.NoUpdate
                )

        GenerateEnvironment ( shuffledBoard, potentialNewLot ) ->
            let
                ssUpdate =
                    potentialNewLot
                        |> Maybe.andThen (generateEnvironment sharedState shuffledBoard)
                        |> Maybe.map (\lot -> SharedState.Replace (SharedState.addLot sharedState lot))
                        |> Maybe.withDefault SharedState.NoUpdate
            in
            ( model, Cmd.none, ssUpdate )

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


prepareRandomDataForGeneration : Board -> List NewLot -> Cmd Msg
prepareRandomDataForGeneration board unusedLots =
    Random.List.choose unusedLots
        -- keep just the random "head"
        |> Random.map Tuple.first
        -- combine it with the shuffled board
        |> Random.map2 Tuple.pair (Random.List.shuffle (Dict.toList board))
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


generateEnvironment : SharedState -> ShuffledBoard -> NewLot -> Maybe Lot
generateEnvironment sharedState shuffledBoard newLot =
    let
        roadOrientation =
            newLot.content.entryDirection
                |> Direction.toOrientation
                |> Direction.oppositeOrientation
    in
    findLotAnchor
        { targetOrientation = roadOrientation
        , targetDirection = Direction.opposite newLot.content.entryDirection
        , newLot = newLot
        , sharedState = sharedState
        , shuffledBoard = shuffledBoard
        }
        |> Maybe.map (Lot.anchorTo newLot)


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
                TwoLaneRoad (Regular orientation) Both ->
                    (orientation == targetOrientation) && hasEnoughSpaceAround cell

                _ ->
                    False

        hasEnoughSpaceAround cell =
            SharedState.isEmptyArea
                { origin = Lot.bottomLeftCorner newLot ( cell, targetDirection )
                , width = newLot.width
                , height = newLot.height
                }
                sharedState
    in
    shuffledBoard
        |> List.filter isCompatible
        |> List.head



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
