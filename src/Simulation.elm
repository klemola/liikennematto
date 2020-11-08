module Simulation exposing (Model, Msg(..), init, subscriptions, update)

import Cell exposing (Cell)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot, NewLot)
import Process
import Random
import Random.List
import Round
import Task
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..), TrafficDirection(..))
import Time
import TrafficLight
import World
    exposing
        ( SimulationState(..)
        , World
        )


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


subscriptions : World -> Sub Msg
subscriptions world =
    case world.simulationState of
        Simulation speed ->
            let
                ( slowTickSpeed, fastTickSpeed ) =
                    World.simulationSpeedValues speed
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


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
        UpdateTraffic _ ->
            ( model
            , updateTraffic model world
            , Cmd.batch
                [ tossACoin
                , shuffleDirections
                ]
            )

        UpdateEnvironment _ ->
            ( model
            , updateEnvironment world
            , Cmd.none
            )

        PrepareGeneration _ ->
            let
                currentLotsAmount =
                    Dict.size world.lots

                existingBuildingKinds =
                    world.lots
                        |> Dict.map (\_ lot -> lot.content.kind)
                        |> Dict.values

                unusedLots =
                    List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) Lot.all
            in
            ( model
            , world
            , -- skip the generation if nothing new can be generated, or if the road network is too small
              if world.simulationState == Paused || List.isEmpty unusedLots || (Dict.size world.board * 4) < currentLotsAmount then
                prepareGenerationAfterRandomDelay

              else
                Cmd.batch
                    [ generateEnvironmentWithRandomData world unusedLots
                    , prepareGenerationAfterRandomDelay
                    ]
            )

        GenerateEnvironment ( shuffledBoard, potentialNewLot ) ->
            let
                nextWorld =
                    potentialNewLot
                        |> Maybe.andThen (generateEnvironment world shuffledBoard)
                        |> Maybe.map (\lot -> World.withNewLot lot world)
                        |> Maybe.withDefault world
            in
            ( model, nextWorld, Cmd.none )

        ReceiveCoinTossResult result ->
            ( { model | coinTossResult = result }, world, Cmd.none )

        ReceiveRandomDirections directions ->
            ( { model | randomDirections = directions }, world, Cmd.none )


shuffleDirections : Cmd Msg
shuffleDirections =
    Direction.all
        |> Random.List.shuffle
        |> Random.generate ReceiveRandomDirections


tossACoin : Cmd Msg
tossACoin =
    Random.weighted ( 60, False ) [ ( 40, True ) ]
        |> Random.generate ReceiveCoinTossResult


generateEnvironmentWithRandomData : World -> List NewLot -> Cmd Msg
generateEnvironmentWithRandomData world unusedLots =
    Random.List.choose unusedLots
        -- keep just the random "head"
        |> Random.map Tuple.first
        -- combine it with the shuffled board
        |> Random.map2 Tuple.pair (Random.List.shuffle (Dict.toList world.board))
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


updateEnvironment : World -> World
updateEnvironment world =
    -- Room for improvement: consider moving traffic light state from tiles to World
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
    world
        |> World.withBoard (Dict.map (\_ tile -> updateTile tile) world.board)


generateEnvironment : World -> ShuffledBoard -> NewLot -> Maybe Lot
generateEnvironment world shuffledBoard newLot =
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
        , world = world
        , shuffledBoard = shuffledBoard
        }
        |> Maybe.map (Lot.anchorTo newLot)


findLotAnchor :
    { targetOrientation : Orientation
    , targetDirection : Direction
    , newLot : NewLot
    , world : World
    , shuffledBoard : ShuffledBoard
    }
    -> Maybe ( Cell, Tile )
findLotAnchor { targetOrientation, targetDirection, newLot, world, shuffledBoard } =
    let
        isCompatible ( cell, tile ) =
            case tile of
                TwoLaneRoad (Regular orientation) Both ->
                    (orientation == targetOrientation) && hasEnoughSpaceAround cell

                _ ->
                    False

        hasEnoughSpaceAround cell =
            World.isEmptyArea
                { origin = Lot.bottomLeftCorner newLot ( cell, targetDirection )
                , width = newLot.width
                , height = newLot.height
                }
                world
    in
    shuffledBoard
        |> List.filter isCompatible
        |> List.head



--
-- Traffic logic (cars)
--


updateTraffic : Model -> World -> World
updateTraffic model world =
    updateTrafficHelper
        { updateQueue = Dict.keys world.cars
        , model = model
        , world = world
        }


updateTrafficHelper :
    { updateQueue : List Int
    , model : Model
    , world : World
    }
    -> World
updateTrafficHelper { updateQueue, model, world } =
    case updateQueue of
        activeCarId :: queue ->
            let
                nextRound updatedCar =
                    updateTrafficHelper
                        { updateQueue = queue
                        , model = model
                        , world =
                            world
                                |> World.withUpdatedCar activeCarId updatedCar
                        }

                -- Room for improvement: only query cars that are nearby
                otherCars =
                    world.cars
                        |> Dict.filter (\k _ -> k /= activeCarId)
                        |> Dict.values
            in
            case Dict.get activeCarId world.cars of
                Just activeCar ->
                    Round.new world model.coinTossResult model.randomDirections activeCar otherCars
                        |> Round.attemptRespawn
                        |> Round.play
                        |> nextRound

                -- this should never happen, but the typesystem doesn't know that
                Nothing ->
                    world

        [] ->
            world
