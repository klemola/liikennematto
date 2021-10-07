module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Liikennematto exposing (SimulationState, Tool)
import Model.Tilemap exposing (Cell, TilemapChange)
import Time


type Message
    = NoOp
    | ResizeWindow Int Int
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | TilemapChanged TilemapChange
    | SetSimulation SimulationState
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
