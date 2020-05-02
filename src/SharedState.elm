module SharedState exposing (..)


type alias SharedState =
    { simulationState : SimulationState
    , simulationSpeed : SimulationSpeed
    }


initial : SharedState
initial =
    { simulationState = Simulation
    , simulationSpeed = Medium
    }


type SimulationState
    = Simulation
    | Paused


nextSimulationState : SimulationState -> SimulationState
nextSimulationState current =
    case current of
        Simulation ->
            Paused

        Paused ->
            Simulation


type SimulationSpeed
    = Slow
    | Medium
    | Fast


simulationSpeedValues : SimulationSpeed -> ( Float, Float )
simulationSpeedValues speed =
    -- (Environment update, Traffic update)
    case speed of
        Slow ->
            ( 1200, 300 )

        Medium ->
            ( 900, 150 )

        Fast ->
            ( 600, 75 )
