module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (SimulationState, Tool)
import Time


type Message
    = ResizeWindow Int Int
    | VisibilityChanged Visibility
    | SetSimulation SimulationState
    | UpdateTraffic Float
    | UpdateEnvironment Time.Posix
    | GenerateEnvironment ()
    | CheckQueues Time.Posix
    | CheckCarStatus Time.Posix
    | SpawnTestCar
    | AddTile Cell
    | RemoveTile Cell
    | ResetWorld
    | SelectTool Tool
    | ToggleDebugMode
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
    | NoOp
