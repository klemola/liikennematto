module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Model.Debug exposing (DebugLayerKind, DevAction, DevOutput)
import Model.World exposing (LotPlacement, TilemapChange)
import Task
import Tilemap.DrivenWFC exposing (RunWFCResult)
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
    | KeyReleased String
    | AudioInitComplete
    | GameSetupComplete
    | ToggleSimulationActive
    | NewGame
    | RestoreGame
      -- Tilemap & Simulation
    | UpdateTraffic Duration
    | CheckQueues Time.Posix Duration
    | UpdateEnvironment
    | UpdateTilemap Duration
    | WFCChunkProcessed RunWFCResult
    | TilemapChanged TilemapChange
    | LotsPlaced (List LotPlacement)
    | TriggerDevAction DevAction
      -- UI
    | InputReceived UI.Core.InputEvent
    | ZoomLevelChanged UI.Core.ZoomLevel
    | ToggleDebugPanel
    | ToggleDebugLayer DebugLayerKind
    | SelectDevOutput DevOutput
    | EditorMsg UI.Editor.Msg
    | ZoomControlMsg UI.ZoomControl.Msg


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
