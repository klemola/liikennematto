module UI.Model exposing
    ( ButtonKind(..)
    , DevView(..)
    , UI
    , ZoomLevel(..)
    , initialModel
    )

import UI.Editor


type alias UI =
    { showMenu : Bool
    , zoomLevel : ZoomLevel
    , showDevMenu : Bool
    , selectedDevView : DevView
    , editor : UI.Editor.Model
    }


type ButtonKind
    = NewGame
    | PauseSimulation
    | ResumeSimulation
    | ZoomIn
    | ZoomOut
    | ToggleCarDebug
    | ToggleLotDebug
    | ToggleGraphDebug
    | SpawnCar


type ZoomLevel
    = Near
    | Far
    | VeryFar


type DevView
    = EventQueueList
    | CarsList
    | WFCOverview


initialModel : UI
initialModel =
    { showMenu = False
    , zoomLevel = Far
    , showDevMenu = False
    , selectedDevView = EventQueueList
    , editor = UI.Editor.initialModel
    }
