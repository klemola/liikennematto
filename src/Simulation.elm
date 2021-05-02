module Simulation exposing
    ( Model
    , Msg(..)
    , SimulationState(..)
    , init
    , maxCarSpawnQueueSize
    , subscriptions
    , update
    )

import Board
import Browser.Events as Events
import Car exposing (Car)
import Cell exposing (Cell)
import Config
import Dict
import Geometry exposing (LMEntityCoordinates)
import Length
import Lot exposing (NewLot)
import Process
import QuadTree exposing (QuadTree)
import Random
import Random.List
import Round
import Task
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
                    updateTraffic world model
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
                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn model.carSpawnQueue model.seed world
            in
            ( { model
                | carSpawnQueue = nextCarSpawnQueue
                , seed = nextSeed
              }
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
    let
        nextTrafficLights =
            world.trafficLights
                |> Dict.map (\_ trafficLight -> TrafficLight.advance trafficLight)
    in
    { world | trafficLights = nextTrafficLights }


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
                |> Maybe.map (\lot -> World.addLot lot world)
                |> Maybe.withDefault world
    in
    ( nextWorld, nextSeed )


findLotAnchor : World -> Random.Seed -> NewLot -> Maybe ( NewLot, Cell )
findLotAnchor world seed newLot =
    let
        ( shuffledBoard, _ ) =
            Random.step (Random.List.shuffle (Dict.toList world.board)) seed

        targetTile =
            if Cell.isVertical newLot.content.entryDirection then
                Board.twoLaneRoadHorizontal

            else
                Board.twoLaneRoadVertical

        targetDirection =
            Cell.oppositeOrthogonalDirection newLot.content.entryDirection

        isCompatible ( cell, tile ) =
            tile == targetTile && hasEnoughSpaceAround cell && not (World.hasLotAnchor cell world)

        lotBoundingBox cell =
            Lot.bottomLeftCorner ( cell, targetDirection ) newLot
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


updateTraffic : World -> Model -> ( World, Random.Seed )
updateTraffic world { seed } =
    let
        cars =
            Dict.values world.cars

        carPositionLookup =
            QuadTree.init Board.boundingBox 4 |> QuadTree.insertList cars
    in
    updateTrafficHelper
        { updateQueue = cars
        , carPositionLookup = carPositionLookup
        , seed = seed
        , world = world
        }


updateTrafficHelper :
    { updateQueue : List Car
    , carPositionLookup : QuadTree Length.Meters LMEntityCoordinates Car
    , seed : Random.Seed
    , world : World
    }
    -> ( World, Random.Seed )
updateTrafficHelper { updateQueue, carPositionLookup, seed, world } =
    case updateQueue of
        [] ->
            ( world, seed )

        activeCar :: queue ->
            let
                nextRound roundResults =
                    updateTrafficHelper
                        { updateQueue = queue
                        , seed = roundResults.seed
                        , carPositionLookup =
                            carPositionLookup
                                |> QuadTree.update
                                    (\_ -> roundResults.car)
                                    activeCar
                        , world = world |> World.setCar activeCar.id roundResults.car
                        }

                otherCars =
                    carPositionLookup
                        |> QuadTree.neighborsWithin Config.tileSizeInMeters activeCar.boundingBox
                        |> List.filter (\car -> car.id /= activeCar.id)

                round =
                    { world = world
                    , activeCar = activeCar
                    , otherCars = otherCars
                    , seed = seed
                    }
            in
            Round.play round
                |> nextRound



--
-- Queues
--


dequeueCarSpawn : Int -> Random.Seed -> World -> ( World, Int, Random.Seed )
dequeueCarSpawn queue seed world =
    let
        canSpawnCar =
            queue > 0
    in
    if canSpawnCar then
        let
            ( nextWorld, nextSeed, newCarId ) =
                World.spawnCar seed world
        in
        case newCarId of
            Just _ ->
                ( nextWorld, queue - 1, nextSeed )

            Nothing ->
                ( nextWorld, queue, nextSeed )

    else
        ( world, queue, seed )
