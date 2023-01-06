module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Car exposing (CarEvent)
import Model.Cell exposing (Cell)
import Model.Debug exposing (DebugLayerKind)
import Model.Entity exposing (Id)
import Model.Liikennematto exposing (SimulationState)
import Task
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
    | TrafficUpdated ( List ( Id, CarEvent ), Time.Posix )
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
    | ShowDotString String


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
