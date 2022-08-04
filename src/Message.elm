module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (SimulationState, Tool)
import Time exposing (Posix)


type Message
    = NoOp
    | ResizeWindow Int Int
    | VisibilityChanged Visibility
    | ResetSeed Posix
    | AnimationFrameReceived Duration
    | AddTile Cell
    | RemoveTile Cell
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
    | SetSimulation SimulationState
    | UpdateEnvironment
    | GenerateEnvironment
    | CheckQueues
    | SpawnTestCar
    | ResetWorld
    | SelectTool Tool
    | ToggleDebugMode
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
