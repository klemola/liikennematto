module Message exposing (Message(..))

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (SimulationState)
import Time exposing (Posix)


type Message
    = NoOp
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | ResetSeed Posix
    | AnimationFrameReceived Duration
    | SetSimulation SimulationState
    | UpdateTraffic Duration
    | UpdateEnvironment
    | GenerateEnvironment
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
    | CheckQueues
    | SpawnTestCar
    | ResetWorld
    | ChangeZoomLevel Float
    | OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | OverlayPointerCancel Pointer.Event
    | ToggleDebugMode
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
