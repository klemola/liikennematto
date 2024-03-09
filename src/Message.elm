module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Debug exposing (DebugLayerKind)
import Task
import Tilemap.Cell exposing (Cell)
import Time
import UI.Core
import UI.Editor
import UI.ZoomControl


type Message
    = NoOp
      -- High level
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | AudioInitComplete
    | GameSetupComplete
    | ToggleSimulationActive
    | NewGame
    | RestoreGame
      -- Simulation / World
    | UpdateTraffic Duration
    | CheckQueues Time.Posix
    | UpdateEnvironment
    | GenerateEnvironment
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
      -- UI / Editor
    | InputReceived UI.Core.InputEvent
    | ZoomLevelChanged UI.Core.ZoomLevel
    | SpawnTestCar
    | ToggleDebugPanel
    | ToggleDebugLayer DebugLayerKind
    | EditorMsg UI.Editor.Msg
    | ZoomControlMsg UI.ZoomControl.Msg


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
