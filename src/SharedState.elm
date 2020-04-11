module SharedState exposing (..)


type Mode
    = Simulation
    | Paused


nextMode : Mode -> Mode
nextMode current =
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
            ( 2000, 1000 )

        Medium ->
            ( 1000, 500 )

        Fast ->
            ( 600, 300 )


type alias SharedState =
    { mode : Mode
    , simulationSpeed : SimulationSpeed
    }


initial : SharedState
initial =
    { mode = Simulation
    , simulationSpeed = Medium
    }
