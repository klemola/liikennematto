module Simulation.Simulation exposing
    ( initCmd
    , update
    )

import Dict
import Duration
import FSM
import Message exposing (Message(..))
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
        AnimationFrameReceived delta ->
            let
                { world } =
                    model

                cars =
                    Dict.values world.cars

                nextWorld =
                    Traffic.updateTraffic
                        { updateQueue = cars
                        , seed = model.seed
                        , world = world
                        , delta = delta
                        }
            in
            ( { model
                | world = nextWorld
                , seed =
                    -- Make sure that the seed is stepped often so that deeply nested functions don't have to return new seeds
                    Random.step (Random.int 0 42) model.seed
                        |> Tuple.second
              }
            , Cmd.none
            )

        SetSimulation simulation ->
            ( { model | simulation = simulation }, Cmd.none )

        TilemapChanged tilemapChange ->
            let
                nextWorld =
                    model.world
                        |> Zoning.removeInvalidLots tilemapChange
                        |> Infrastructure.updateRoadNetwork
                        |> Traffic.rerouteCarsIfNeeded
            in
            ( { model | world = nextWorld }
            , Cmd.none
            )

        UpdateEnvironment ->
            ( { model | world = updateTrafficLights model.world }
            , Cmd.none
            )

        GenerateEnvironment ->
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
