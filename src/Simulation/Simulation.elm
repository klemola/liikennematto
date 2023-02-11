module Simulation.Simulation exposing
    ( update
    , worldAfterTilemapChange
    )

import Audio exposing (playSound)
import Collection
import Duration
import FSM
import Message exposing (Message(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        , SimulationState(..)
        )
import Model.RenderCache exposing (refreshTilemapCache)
import Model.Tile as Tile
import Model.Tilemap as Tilemap
import Model.TrafficLight exposing (TrafficLight)
import Model.World as World exposing (World)
import Process
import Random
import Simulation.Events exposing (updateEventQueue)
import Simulation.Infrastructure as Infrastructure
import Simulation.Traffic as Traffic
import Simulation.Zoning as Zoning
import Task
import Time



--
-- Core update function and effects
--


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        GameSetupComplete ->
            ( model, generateEnvironmentAfterDelay model.world )

        UpdateTraffic delta ->
            let
                cars =
                    -- TODO: fold to avoid double iteration
                    Collection.values model.world.cars

                ( worldWithTrafficUpdate, worldEvents ) =
                    Traffic.updateTraffic
                        { updateQueue = cars
                        , world = model.world
                        , delta = delta
                        , events = []
                        }
            in
            ( { model
                | world =
                    processWorldEvents
                        model.time
                        worldEvents
                        worldWithTrafficUpdate
              }
            , Cmd.none
            )

        CheckQueues time ->
            ( { model
                | world =
                    model.world
                        |> World.setSeed (Random.initialSeed (Time.posixToMillis time))
                        |> updateEventQueue time
                , time = time
              }
            , Cmd.none
            )

        SetSimulation simulation ->
            ( { model | simulation = simulation }, Cmd.none )

        UpdateTilemap delta ->
            let
                { world, renderCache } =
                    model

                tilemapUpdateResult =
                    Tilemap.update delta world.tilemap

                ( nextWorld, changedCells ) =
                    { world | tilemap = tilemapUpdateResult.tilemap }
                        |> Zoning.removeInvalidLots tilemapUpdateResult.transitionedCells
                        |> World.resolveTilemapUpdate delta tilemapUpdateResult

                tilemapChangedEffects =
                    if List.isEmpty changedCells then
                        Cmd.none

                    else
                        Task.succeed changedCells
                            |> Task.perform TilemapChanged

                ( nextRenderCache, dynamicTiles ) =
                    refreshTilemapCache tilemapUpdateResult renderCache
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
                , dynamicTiles = dynamicTiles
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds tilemapUpdateResult.actions)
            )

        TilemapChanged _ ->
            ( { model | world = worldAfterTilemapChange model.world }
            , Cmd.none
            )

        UpdateEnvironment ->
            ( { model | world = updateTrafficLights model.world }
            , Cmd.none
            )

        GenerateEnvironment ->
            let
                nextWorld =
                    attemptGenerateLot
                        model.time
                        model.simulation
                        model.world

                cmds =
                    if Collection.size nextWorld.lots > Collection.size model.world.lots then
                        Cmd.batch
                            [ generateEnvironmentAfterDelay nextWorld
                            , Audio.playSound Audio.BuildLot
                            ]

                    else
                        generateEnvironmentAfterDelay nextWorld
            in
            ( { model | world = nextWorld }
            , cmds
            )

        _ ->
            ( model, Cmd.none )


generateEnvironmentAfterDelay : World -> Cmd Message
generateEnvironmentAfterDelay world =
    let
        randomMillis =
            world.seed
                |> Random.step (Random.int 1000 3500)
                |> Tuple.first
    in
    randomMillis
        |> toFloat
        |> Process.sleep
        |> Task.perform (always GenerateEnvironment)


tileActionsToCmds : List Tile.Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                Tile.PlayAudio sound ->
                    playSound sound
        )



--
-- World effects
--


processWorldEvents : Time.Posix -> List World.WorldEvent -> World -> World
processWorldEvents time events world =
    List.foldl
        (\event nextWorld ->
            if event == World.None then
                nextWorld

            else
                -- Trigger on next update
                World.addEvent event time nextWorld
        )
        world
        events


worldAfterTilemapChange : World -> World
worldAfterTilemapChange world =
    world
        |> Infrastructure.updateRoadNetwork
        |> Traffic.rerouteCarsIfNeeded



--
-- Environment and zoning
--


updateTrafficLights : World -> World
updateTrafficLights world =
    { world
        | trafficLights =
            Collection.map
                (\_ trafficLight -> updateTrafficLight trafficLight)
                world.trafficLights
    }


updateTrafficLight : TrafficLight -> TrafficLight
updateTrafficLight trafficLight =
    let
        -- FSM change actions are ignored
        ( nextFsm, _ ) =
            FSM.updateWithoutContext (Duration.seconds 1) trafficLight.fsm
    in
    { trafficLight | fsm = nextFsm }


attemptGenerateLot : Time.Posix -> SimulationState -> World -> World
attemptGenerateLot time simulation world =
    let
        largeEnoughRoadNetwork =
            Tilemap.size world.tilemap > 4 * (Collection.size world.lots + 1)
    in
    if simulation == Paused || not largeEnoughRoadNetwork || World.hasPendingTilemapChange world then
        world

    else
        Zoning.generateLot time world
