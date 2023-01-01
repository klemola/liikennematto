module Simulation.Simulation exposing
    ( update
    , worldAfterTilemapChange
    )

import Audio
import Dict
import Duration
import FSM
import Message exposing (Message(..))
import Model.Editor as Editor
import Model.Liikennematto
    exposing
        ( Liikennematto
        , SimulationState(..)
        )
import Model.Tilemap as Tilemap
import Model.TrafficLight exposing (TrafficLight)
import Model.World exposing (World)
import Process
import Random
import Simulation.Events exposing (processEvents, updateEventQueue)
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
            ( model, generateEnvironmentAfterDelay model.seed )

        UpdateTraffic delta ->
            let
                { world } =
                    model

                cars =
                    -- TODO: fold to avoid double iteration
                    Dict.values world.cars

                ( nextWorld, trafficEvents ) =
                    Traffic.updateTraffic
                        { updateQueue = cars
                        , seed = model.seed
                        , roadNetworkStale = Editor.hasPendingTilemapChange model.editor
                        , world = world
                        , delta = delta
                        , events = []
                        }
            in
            ( { model | world = nextWorld }
            , Time.now
                |> Task.map (Tuple.pair trafficEvents)
                |> Task.perform TrafficUpdated
            )

        TrafficUpdated ( trafficEvents, time ) ->
            let
                nextWorld =
                    processEvents time trafficEvents model.world
            in
            ( { model
                | world = nextWorld
                , time = time
                , seed = Random.initialSeed (Time.posixToMillis time)
              }
            , Cmd.none
            )

        CheckQueues time ->
            ( { model | world = updateEventQueue time model.seed model.world }
            , Cmd.none
            )

        SetSimulation simulation ->
            ( { model | simulation = simulation }, Cmd.none )

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
                        model.seed
                        model.editor
                        model.simulation
                        model.world

                cmds =
                    if Dict.size nextWorld.lots > Dict.size model.world.lots then
                        Cmd.batch
                            [ generateEnvironmentAfterDelay model.seed
                            , Audio.playSound Audio.BuildLot
                            ]

                    else
                        generateEnvironmentAfterDelay model.seed
            in
            ( { model | world = nextWorld }
            , cmds
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
        |> Task.perform (always GenerateEnvironment)


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
    let
        nextTrafficLights =
            Dict.map
                (\_ trafficLight -> updateTrafficLight trafficLight)
                world.trafficLights
    in
    { world | trafficLights = nextTrafficLights }


updateTrafficLight : TrafficLight -> TrafficLight
updateTrafficLight trafficLight =
    let
        -- FSM change actions are ignored
        ( nextFsm, _ ) =
            FSM.updateWithoutContext (Duration.seconds 1) trafficLight.fsm
    in
    { trafficLight | fsm = nextFsm }


attemptGenerateLot : Time.Posix -> Random.Seed -> Editor.Editor -> SimulationState -> World -> World
attemptGenerateLot time seed editor simulation world =
    let
        largeEnoughRoadNetwork =
            Tilemap.size world.tilemap > 4 * (Dict.size world.lots + 1)
    in
    if simulation == Paused || not largeEnoughRoadNetwork || editor.pendingTilemapChange /= Nothing then
        world

    else
        Zoning.generateLot time seed world
