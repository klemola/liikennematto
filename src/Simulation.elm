module Simulation exposing
    ( Model
    , Msg(..)
    , SimulationState(..)
    , init
    , maxCarSpawnQueueSize
    , subscriptions
    , update
    )

import BoundingBox2d
import Browser.Events as Events
import Car
import Cell exposing (Cell)
import Config
import Dict
import Direction2d
import Geometry
import Lot exposing (NewLot)
import Process
import Random
import Random.List
import RoadNetwork exposing (RNNodeContext)
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
import World exposing (World)


type alias Model =
    { seed : Random.Seed
    , simulation : SimulationState
    , carSpawnQueue : Int
    }


type SimulationState
    = Running
    | Paused


type Msg
    = SetSimulation SimulationState
    | UpdateTraffic Time.Posix
    | UpdateEnvironment Time.Posix
    | GenerateEnvironment ()
    | CheckQueues Time.Posix
    | SpawnTestCar


init : ( Model, Cmd Msg )
init =
    let
        seed =
            Random.initialSeed 666
    in
    ( { seed = seed
      , simulation = Running
      , carSpawnQueue = 0
      }
    , generateEnvironmentAfterDelay seed
    )


maxCarSpawnQueueSize : Int
maxCarSpawnQueueSize =
    5


subscriptions : Model -> Sub Msg
subscriptions { simulation } =
    if simulation == Paused then
        Sub.none

    else
        Sub.batch
            [ Time.every Config.environmentUpdateFrequency UpdateEnvironment
            , Time.every Config.dequeueFrequency CheckQueues
            , Events.onAnimationFrame UpdateTraffic
            ]



--
-- Core update function and effects
--


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
        SetSimulation simulation ->
            ( { model | simulation = simulation }, world, Cmd.none )

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
                    attemptGenerateEnvironment world model.seed model.simulation
            in
            ( { model | seed = nextSeed }
            , nextWorld
            , generateEnvironmentAfterDelay nextSeed
            )

        CheckQueues _ ->
            let
                ( nextCarSpawnQueue, nextWorld ) =
                    dequeueCarSpawn model.carSpawnQueue model.seed world
            in
            ( { model | carSpawnQueue = nextCarSpawnQueue }
            , nextWorld
            , Cmd.none
            )

        SpawnTestCar ->
            ( { model | carSpawnQueue = model.carSpawnQueue + 1 }
            , world
            , Cmd.none
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


attemptGenerateEnvironment : World -> Random.Seed -> SimulationState -> ( World, Random.Seed )
attemptGenerateEnvironment world seed simulation =
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
    if simulation == Paused || List.isEmpty unusedLots || not largeEnoughRoadNetwork then
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
                -- TODO: change, this is stupid
                |> Tile.toOrientation
                |> Tile.oppositeOrientation

        targetDirection =
            Cell.oppositeOrthogonalDirection newLot.content.entryDirection

        isCompatible ( cell, tile ) =
            case tile of
                TwoLaneRoad (Regular orientation) Both ->
                    (orientation == targetOrientation) && hasEnoughSpaceAround cell && not (World.hasLotAnchor cell world)

                _ ->
                    False

        lotBoundingBox cell =
            Lot.bottomLeftCorner newLot ( cell, targetDirection )
                |> Geometry.boundingBoxWithDimensions newLot.width newLot.height

        hasEnoughSpaceAround cell =
            World.isEmptyArea (lotBoundingBox cell) world
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



-- Queues


dequeueCarSpawn : Int -> Random.Seed -> World -> ( Int, World )
dequeueCarSpawn queue seed world =
    let
        canSpawnCar =
            queue > 0
    in
    if canSpawnCar then
        RoadNetwork.getRandomNode world.roadNetwork seed
            |> Maybe.andThen (validateSpawnConditions world)
            |> Maybe.map (spawnCar world)
            |> Maybe.map (Tuple.pair <| queue - 1)
            |> Maybe.withDefault ( queue, world )

    else
        ( queue, world )


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            Car.boundingBox car
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            Dict.size world.board > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing


spawnCar : World -> RNNodeContext -> World
spawnCar world nodeCtx =
    let
        car =
            Car.new Car.TestCar
                |> Car.withPosition nodeCtx.node.label.position
                |> Car.withRotation (Direction2d.toAngle nodeCtx.node.label.direction)
                |> Car.withRoute nodeCtx
    in
    world
        |> World.withCar car
