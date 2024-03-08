module Simulation.Update exposing (update, worldAfterTilemapChange)

import Audio
import Collection
import Duration
import FSM
import Message exposing (Message(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        )
import Model.World as World exposing (World, updateRoadNetwork)
import Process
import Random
import Simulation.Events exposing (updateEventQueue)
import Simulation.Traffic as Traffic exposing (rerouteCarsIfNeeded)
import Simulation.TrafficLight exposing (TrafficLight)
import Simulation.Zoning exposing (generateLot)
import Task
import Tilemap.Core exposing (tilemapSize)
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
                        model.simulationActive
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

        SpawnTestCar ->
            ( { model
                | world =
                    World.addEvent
                        World.SpawnTestCar
                        model.time
                        model.world
              }
            , Cmd.none
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
        |> updateRoadNetwork
        |> rerouteCarsIfNeeded



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


attemptGenerateLot : Time.Posix -> Bool -> World -> World
attemptGenerateLot time simulationActive world =
    let
        largeEnoughRoadNetwork =
            tilemapSize world.tilemap > 4 * (Collection.size world.lots + 1)
    in
    if not simulationActive || not largeEnoughRoadNetwork || World.hasPendingTilemapChange world then
        world

    else
        generateLot time world
