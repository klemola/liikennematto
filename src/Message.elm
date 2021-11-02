module Message exposing (Message(..))

import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Liikennematto exposing (SimulationState, Tool)
import Model.Tilemap exposing (Cell)


type Message
    = NoOp
    | ResizeWindow Int Int
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | AddTile Cell
    | RemoveTile Cell
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
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
