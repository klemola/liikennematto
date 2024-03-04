module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Debug exposing (DebugLayerKind)
import Model.Liikennematto exposing (SimulationState)
import Task
import Tilemap.Cell exposing (Cell)
import Time


type Message
    = NoOp
      -- High level
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | AnimationFrameReceived Duration
    | AudioInitComplete
    | GameSetupComplete
    | InGame
    | NewGame
    | RestoreGame
      -- Simulation / World
    | SetSimulation SimulationState
    | UpdateTraffic Duration
    | CheckQueues Time.Posix
    | UpdateEnvironment
    | GenerateEnvironment
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
    | SpawnTestCar
      -- UI / Editor
    | ChangeZoomLevel Float
    | OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | OverlayPointerCancel Pointer.Event
    | ToggleDebugPanel
    | ToggleDebugLayer DebugLayerKind


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
