module Simulation.Simulation exposing
    ( initCmd
    , update
    )

import Dict
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..), Tool(..))
import Model.Tilemap as Tilemap
import Model.TrafficLight as TrafficLight
import Model.World as World exposing (World)
import Process
import Random
import Simulation.Infrastructure as Infrastructure
import Simulation.Traffic as Traffic
import Simulation.Zoning as Zoning
import Task


initCmd : Random.Seed -> Cmd Message
initCmd seed =
    generateEnvironmentAfterDelay seed



--
-- Core update function and effects
--


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        SetSimulation simulation ->
            ( { model | simulation = simulation }, Cmd.none )

        AddTile cell ->
            let
                worldWithTilemapChange =
                    Infrastructure.buildRoadAt cell model.world

                nextWorld =
                    { worldWithTilemapChange | cars = Traffic.rerouteCarsIfNeeded worldWithTilemapChange }
            in
            ( { model | world = nextWorld }
            , Cmd.none
            )

        RemoveTile cell ->
            let
                worldWithTilemapChange =
                    Infrastructure.removeRoadAt cell model.world

                nextWorld =
                    { worldWithTilemapChange | cars = Traffic.rerouteCarsIfNeeded worldWithTilemapChange }
            in
            ( { model | world = nextWorld }
            , Cmd.none
            )

        ResetWorld ->
            ( { model
                | tool = SmartConstruction
                , world = World.empty
              }
            , Cmd.none
            )

        UpdateTraffic rafDelta ->
            let
                cars =
                    Dict.values model.world.cars

                ( nextWorld, nextSeed ) =
                    Traffic.updateTraffic
                        { updateQueue = cars
                        , seed = model.seed
                        , world = model.world
                        , delta = rafDelta
                        }
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , Cmd.none
            )

        UpdateEnvironment _ ->
            ( { model | world = updateTrafficLights model.world }
            , Cmd.none
            )

        GenerateEnvironment _ ->
            let
                ( nextWorld, nextSeed ) =
                    attemptGenerateLot model.world model.seed model.simulation
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , generateEnvironmentAfterDelay nextSeed
            )

        CheckQueues _ ->
            let
                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn model.carSpawnQueue model.seed model.world
            in
            ( { model
                | carSpawnQueue = nextCarSpawnQueue
                , world = nextWorld
                , seed = nextSeed
              }
            , Cmd.none
            )

        CheckCarStatus _ ->
            let
                ( nextWorld, nextSeed ) =
                    Traffic.checkCarStatus model.seed model.world
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , Cmd.none
            )

        SpawnTestCar ->
            ( { model | carSpawnQueue = model.carSpawnQueue + 1 }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


generateEnvironmentAfterDelay : Random.Seed -> Cmd Message
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
-- Environment and zoning
--


updateTrafficLights : World -> World
updateTrafficLights world =
    let
        nextTrafficLights =
            world.trafficLights
                |> Dict.map (\_ trafficLight -> TrafficLight.advance trafficLight)
    in
    { world | trafficLights = nextTrafficLights }


attemptGenerateLot : World -> Random.Seed -> SimulationState -> ( World, Random.Seed )
attemptGenerateLot world seed simulation =
    let
        largeEnoughRoadNetwork =
            Tilemap.size world.tilemap > 4 * max 1 (Dict.size world.lots + 1)
    in
    if simulation == Paused || not largeEnoughRoadNetwork then
        ( world, seed )

    else
        Zoning.generateLot world seed



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
                Traffic.spawnCar seed world
        in
        case newCarId of
            Just _ ->
                ( nextWorld, queue - 1, nextSeed )

            Nothing ->
                ( nextWorld, queue, nextSeed )

    else
        ( world, queue, seed )
