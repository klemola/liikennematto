module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Liikennematto exposing (SimulationState, Tool)
import Model.Tilemap exposing (Cell, TilemapChange)


type Message
    = NoOp
    | ResizeWindow Int Int
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | AddTile Cell
    | RemoveTile Cell
    | TilemapChanged TilemapChange
    | SetSimulation SimulationState
    | UpdateEnvironment
    | GenerateEnvironment
    | CheckQueues
    | CheckCarStatus
    | SpawnTestCar
    | ResetWorld
    | SelectTool Tool
    | ToggleDebugMode
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
