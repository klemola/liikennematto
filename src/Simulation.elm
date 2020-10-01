module Simulation exposing (Model, Msg(..), initialModel, subscriptions, update)

import Board exposing (Board)
import Car
import Config
import Coords exposing (Coords)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot(..), allBuildingKinds)
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
    List ( Coords, Tile )


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
            ( model
            , Cmd.none
            , generateEnvironment
                { board = sharedState.board
                , shuffledBoard = shuffledBoard
                , cars = sharedState.cars
                , lots = sharedState.lots
                }
                |> SharedState.UpdateLots
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


generateEnvironment :
    { board : Board
    , shuffledBoard : ShuffledBoard
    , lots : Lots
    , cars : Cars
    }
    -> ( Lots, Cars )
generateEnvironment { board, shuffledBoard, lots, cars } =
    -- early exit if nothing unique can be generated, or if the road network is too small
    if Dict.size lots == List.length allBuildingKinds || Dict.size board < 5 then
        ( lots, cars )

    else
        let
            existingBuildings =
                lots
                    |> Dict.map (\_ (Building kind _) -> kind)
                    |> Dict.values

            -- Room for improvement: buildings should be shuffled so that they don't have to be built in certain order
            nextUnusedBuilding =
                allBuildingKinds
                    |> List.filter
                        (\building ->
                            not (List.member building existingBuildings)
                        )
                    |> List.head

            roadOrientation building =
                Lot.entryDirection building
                    |> Direction.toOrientation
                    |> Direction.oppositeOrientation

            existingLots =
                lots
                    |> Dict.map (\_ lot -> Lot.coords lot)
                    |> Dict.values
        in
        case nextUnusedBuilding of
            Just building ->
                findLotAnchor
                    { targetOrientation = roadOrientation building
                    , targetDirection = Lot.anchorDirection building
                    , board = board
                    , shuffledBoard = shuffledBoard
                    , existingLots = existingLots
                    }
                    |> Maybe.map (Lot.anchorTo building >> addLot lots cars)
                    |> Maybe.withDefault ( lots, cars )

            Nothing ->
                ( lots, cars )


findLotAnchor :
    { targetOrientation : Orientation
    , targetDirection : Direction
    , board : Board
    , shuffledBoard : ShuffledBoard
    , existingLots : List Coords
    }
    -> Maybe ( Coords, Tile )
findLotAnchor { targetOrientation, targetDirection, board, shuffledBoard, existingLots } =
    let
        isCompatible ( coords, tile ) =
            case tile of
                TwoLaneRoad (Regular orientation) _ ->
                    let
                        potentialLotCoords =
                            Coords.next coords targetDirection
                    in
                    (orientation == targetOrientation)
                        && Board.inBounds potentialLotCoords
                        && not (List.member potentialLotCoords existingLots)
                        && not (Board.exists potentialLotCoords board)

                _ ->
                    False
    in
    shuffledBoard
        |> List.filter isCompatible
        |> List.head


addLot : Lots -> Cars -> ( Lot, Coords ) -> ( Lots, Cars )
addLot lots cars ( newLot, _ ) =
    let
        nextLotId =
            SharedState.nextId lots

        nextCarId =
            SharedState.nextId cars

        resident =
            Config.resident newLot

        newLots =
            Dict.insert nextLotId newLot lots

        newCars =
            case newLot of
                Building kind _ ->
                    Dict.insert nextCarId
                        { kind = resident
                        , coords = Lot.coords newLot
                        , direction =
                            Lot.entryDirection kind
                                |> Direction.next
                        , homeLotId = Just nextLotId
                        , status = Car.ParkedAtLot
                        }
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
