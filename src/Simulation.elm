module Simulation exposing (Model, Msg(..), init, subscriptions, update)

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot, NewLot)
import Process
import Random
import Random.List
import Round
import SharedState
    exposing
        ( Cars
        , Lots
        , SharedState
        , SimulationState(..)
        )
import Task
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
    | PrepareGeneration ()
    | GenerateEnvironment ( ShuffledBoard, Maybe NewLot )
    | ReceiveCoinTossResult Bool
    | ReceiveRandomDirections (List Direction)


init : ( Model, Cmd Msg )
init =
    ( { coinTossResult = False
      , randomDirections = Direction.all
      }
    , prepareGenerationAfterRandomDelay
    )


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
                ]

        Paused ->
            Sub.none



--
-- Core update function and effects
--


update : SharedState -> Msg -> Model -> ( Model, SharedState, Cmd Msg )
update sharedState msg model =
    case msg of
        UpdateTraffic _ ->
            ( model
            , sharedState
                |> SharedState.setCars (updateTraffic model sharedState.board sharedState.cars)
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            )

        UpdateEnvironment _ ->
            ( model
            , sharedState
                |> SharedState.setBoard (updateEnvironment sharedState.board)
            , Cmd.none
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
            ( model
            , sharedState
            , -- skip the generation if nothing new can be generated, or if the road network is too small
              if sharedState.simulationState == Paused || List.isEmpty unusedLots || (Dict.size sharedState.board * 4) < currentLotsAmount then
                prepareGenerationAfterRandomDelay

              else
                Cmd.batch
                    [ generateEnvironmentWithRandomData sharedState.board unusedLots
                    , prepareGenerationAfterRandomDelay
                    ]
            )

        GenerateEnvironment ( shuffledBoard, potentialNewLot ) ->
            let
                nextSharedState =
                    potentialNewLot
                        |> Maybe.andThen (generateEnvironment sharedState shuffledBoard)
                        |> Maybe.map (\lot -> SharedState.addLot lot sharedState)
                        |> Maybe.withDefault sharedState
            in
            ( model, nextSharedState, Cmd.none )

        ReceiveCoinTossResult result ->
            ( { model | coinTossResult = result }, sharedState, Cmd.none )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, sharedState, Cmd.none )


shuffleDirections : Cmd Msg
shuffleDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


tossACoin : Cmd Msg
tossACoin =
    Random.weighted ( 60, False ) [ ( 40, True ) ]
        |> Random.generate ReceiveCoinTossResult


generateEnvironmentWithRandomData : Board -> List NewLot -> Cmd Msg
generateEnvironmentWithRandomData board unusedLots =
    Random.List.choose unusedLots
        -- keep just the random "head"
        |> Random.map Tuple.first
        -- combine it with the shuffled board
        |> Random.map2 Tuple.pair (Random.List.shuffle (Dict.toList board))
        |> Random.generate GenerateEnvironment


prepareGenerationAfterRandomDelay : Cmd Msg
prepareGenerationAfterRandomDelay =
    let
        randomMillis =
            Random.int 1000 3500
    in
    Time.now
        |> Task.map
            (Time.posixToMillis
                >> Random.initialSeed
                >> Random.step randomMillis
                >> Tuple.first
            )
        |> Task.andThen (toFloat >> Process.sleep)
        |> Task.perform PrepareGeneration



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
