module SharedState exposing (..)

import Board exposing (Board)
import Car exposing (Car)
import Config exposing (initialCars, initialIntersections, initialRoads)
import Dict exposing (Dict)


type alias SharedState =
    { simulationState : SimulationState
    , simulationSpeed : SimulationSpeed
    , board : Board
    , cars : Cars
    }


type alias Cars =
    Dict Int Car


type SimulationSpeed
    = Slow
    | Medium
    | Fast


type SimulationState
    = Simulation
    | Paused


type SharedStateUpdate
    = NoUpdate
    | UpdateSimulationState SimulationState
    | UpdateSimulationSpeed SimulationSpeed
    | UpdateBoard Board
    | UpdateCars Cars


initial : SharedState
initial =
    let
        board =
            initialRoads
                |> List.append initialIntersections
                |> Dict.fromList
    in
    { simulationState = Simulation
    , simulationSpeed = Medium
    , board = board
    , cars = initialCars
    }


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateSimulationState state ->
            { sharedState | simulationState = state }

        UpdateSimulationSpeed speed ->
            { sharedState | simulationSpeed = speed }

        UpdateBoard board ->
            { sharedState | board = board }

        UpdateCars cars ->
            { sharedState | cars = cars }

        NoUpdate ->
            sharedState


nextSimulationState : SimulationState -> SimulationState
nextSimulationState current =
    case current of
        Simulation ->
            Paused

        Paused ->
            Simulation


simulationSpeedValues : SimulationSpeed -> ( Float, Float )
simulationSpeedValues speed =
    -- (Environment update, Traffic update)
    case speed of
        Slow ->
            ( 1200, 400 )

        Medium ->
            ( 900, 100 )

        Fast ->
            ( 600, 50 )
