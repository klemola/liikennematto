module Simulation exposing (Model, Msg(..), init, subscriptions, update)

import Browser.Events as Events
import Cell exposing (Cell)
import Dict
import Direction exposing (Direction(..), Orientation)
import Lot exposing (Lot, NewLot)
import Process
import Random
import Random.List
import Round
import Task
import Tile
    exposing
        ( IntersectionControl(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import Time
import TrafficLight
import World
    exposing
        ( SimulationState(..)
        , World
        )


type alias Model =
    { seed : Random.Seed
    }


type alias ShuffledBoard =
    List ( Cell, Tile )


type Msg
    = UpdateTraffic Time.Posix
    | UpdateEnvironment Time.Posix
    | GenerateEnvironment ()


init : ( Model, Cmd Msg )
init =
    let
        seed =
            Random.initialSeed 666
    in
    ( { seed = seed }, generateEnvironmentAfterDelay seed )


subscriptions : World -> Sub Msg
subscriptions { simulationState } =
    if simulationState == Paused then
        Sub.none

    else
        Sub.batch
            [ Time.every (environmentUpdateInterval simulationState) UpdateEnvironment
            , Events.onAnimationFrame UpdateTraffic
            ]


environmentUpdateInterval : SimulationState -> Float
environmentUpdateInterval speed =
    case speed of
        RunningAtSlowSpeed ->
            1400

        RunningAtNormalSpeed ->
            700

        Paused ->
            0



--
-- Core update function and effects
--


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
        UpdateTraffic _ ->
            let
                ( nextWorld, nextSeed ) =
                    updateTraffic world model.seed
            in
            ( { model | seed = nextSeed }
            , nextWorld
            , Cmd.none
            )

        UpdateEnvironment _ ->
            ( model
            , updateEnvironment world
            , Cmd.none
            )

        GenerateEnvironment _ ->
            let
                ( nextWorld, nextSeed ) =
                    attemptGenerateEnvironment world model.seed
            in
            ( { model | seed = nextSeed }
            , nextWorld
            , generateEnvironmentAfterDelay nextSeed
            )


generateEnvironmentAfterDelay : Random.Seed -> Cmd Msg
generateEnvironmentAfterDelay seed =
    let
        randomMillis =
            seed
                |> Random.step (Random.int 1000 3500)
                |> Tuple.first
    in
    randomMillis
        |> toFloat
        |> Process.sleep
        |> Task.perform GenerateEnvironment



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


attemptGenerateEnvironment : World -> Random.Seed -> ( World, Random.Seed )
attemptGenerateEnvironment world seed =
    let
        largeEnoughRoadNetwork =
            Dict.size world.board > 4 * max 1 (Dict.size world.lots + 1)

        existingBuildingKinds =
            world.lots
                |> Dict.map (\_ lot -> lot.content.kind)
                |> Dict.values

        unusedLots =
            List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) Lot.all
    in
    if world.simulationState == Paused || List.isEmpty unusedLots || not largeEnoughRoadNetwork then
        ( world, seed )

    else
        generateEnvironment world seed unusedLots


generateEnvironment : World -> Random.Seed -> List NewLot -> ( World, Random.Seed )
generateEnvironment world seed unusedLots =
    let
        randomLot =
            unusedLots
                |> Random.List.choose
                |> Random.map Tuple.first

        ( potentialNewLot, nextSeed ) =
            Random.step randomLot seed

        nextWorld =
            potentialNewLot
                |> Maybe.andThen (findLotAnchor world seed)
                |> Maybe.map Lot.fromNewLot
                |> Maybe.map (\lot -> World.withLot lot world)
                |> Maybe.withDefault world
    in
    ( nextWorld, nextSeed )


findLotAnchor : World -> Random.Seed -> NewLot -> Maybe ( NewLot, Cell )
findLotAnchor world seed newLot =
    let
        ( shuffledBoard, _ ) =
            Random.step (Random.List.shuffle (Dict.toList world.board)) seed

        targetOrientation =
            newLot.content.entryDirection
                |> Direction.toOrientation
                |> Direction.oppositeOrientation

        targetDirection =
            Direction.opposite newLot.content.entryDirection

        isCompatible ( cell, tile ) =
            case tile of
                TwoLaneRoad (Regular orientation) Both ->
                    (orientation == targetOrientation) && hasEnoughSpaceAround cell && not (World.hasLotAnchor cell world)

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
        |> Maybe.map (\( cell, _ ) -> ( newLot, cell ))



--
-- Traffic logic (cars)
--


updateTraffic : World -> Random.Seed -> ( World, Random.Seed )
updateTraffic world seed =
    updateTrafficHelper
        { updateQueue = Dict.keys world.cars
        , seed = seed
        , world = world
        }


updateTrafficHelper :
    { updateQueue : List Int
    , seed : Random.Seed
    , world : World
    }
    -> ( World, Random.Seed )
updateTrafficHelper { updateQueue, seed, world } =
    case updateQueue of
        activeCarId :: queue ->
            let
                nextRound ( updatedCar, nextSeed ) =
                    updateTrafficHelper
                        { updateQueue = queue
                        , seed = nextSeed
                        , world =
                            world
                                |> World.setCar activeCarId updatedCar
                        }

                -- Room for improvement: only query cars that are nearby
                otherCars =
                    world.cars
                        |> Dict.filter (\k _ -> k /= activeCarId)
                        |> Dict.values
            in
            case Dict.get activeCarId world.cars of
                Just activeCar ->
                    Round.new world seed activeCar otherCars
                        |> Round.play
                        |> nextRound

                -- this should never happen, but the typesystem doesn't know that
                Nothing ->
                    ( world, seed )

        [] ->
            ( world, seed )
