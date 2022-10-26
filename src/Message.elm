module Message exposing (Message(..))

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Cell exposing (Cell)
import Model.Editor exposing (Tool)
import Model.Liikennematto exposing (SimulationState)
import Time exposing (Posix)


type Message
    = NoOp
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | ResetSeed Posix
    | AnimationFrameReceived Duration
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
    | SetSimulation SimulationState
    | UpdateEnvironment
    | GenerateEnvironment
    | CheckQueues
    | SpawnTestCar
    | SelectTool Tool
    | ChangeZoomLevel Float
    | OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | ToggleDebugMode
    | ToggleShowRoadNetwork
    | ToggleShowCarDebugVisuals
    | ShowDotString String
    | HideDotString
