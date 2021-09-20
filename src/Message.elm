module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (SimulationState, Tool)
import Time


type Message
    = ResizeWindow Int Int
    | VisibilityChanged Visibility
    | ToggleDebugMode
    | SetSimulation SimulationState
    | UpdateTraffic Float
    | UpdateEnvironment Time.Posix
    | GenerateEnvironment ()
    | CheckQueues Time.Posix
    | CheckCarStatus Time.Posix
    | SpawnTestCar
    | SelectTile Cell
    | SecondaryAction Cell
    | SelectTool Tool
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
