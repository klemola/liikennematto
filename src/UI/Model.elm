module UI.Model exposing
    ( ButtonKind(..)
    , DevView(..)
    , UI
    , ZoomLevel(..)
    , initialModel
    )

import Html.Events.Extra.Pointer as Pointer
import UI.Editor


type alias UI =
    { showMenu : Bool
    , zoomLevel : ZoomLevel
    , showDevMenu : Bool
    , selectedDevView : DevView
    , editor : UI.Editor.Model
    , showLmInfo : Bool
    , lastEventDevice : Pointer.DeviceType
    }


type ButtonKind
    = NewGame
    | PauseSimulation
    | ResumeSimulation
    | ToggleCarDebug
    | ToggleLotDebug
    | ToggleRoadNetworkDebug
    | SpawnCar


type ZoomLevel
    = Near
    | Far
    | VeryFar


type DevView
    = EventQueueList
    | CarsList
    | WFCOverview
    | SavegameOutput


initialModel : UI
initialModel =
    { showMenu = False
    , zoomLevel = Far
    , showDevMenu = False
    , selectedDevView = EventQueueList
    , editor = UI.Editor.initialModel
    , showLmInfo = False
    , lastEventDevice = Pointer.MouseType
    }
