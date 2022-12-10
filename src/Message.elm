module Message exposing (Message(..), asCmd)

import Browser.Dom
import Browser.Events exposing (Visibility)
import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (SimulationState)
import Task
import Time exposing (Posix)


type Message
    = NoOp
    | ResizeTriggered
    | WindowResized Browser.Dom.Viewport
    | VisibilityChanged Visibility
    | ResetSeed Posix
    | AnimationFrameReceived Duration
    | AudioInitComplete
    | GameSetupComplete
    | InGame
    | NewGame
    | RestoreGame
    | SetSimulation SimulationState
    | UpdateTraffic Duration
    | UpdateEnvironment
    | GenerateEnvironment
    | UpdateTilemap Duration
    | TilemapChanged (List Cell)
    | CheckQueues
    | SpawnTestCar
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


asCmd : Message -> Cmd Message
asCmd message =
    Task.perform
        (always message)
        (Task.succeed ())
