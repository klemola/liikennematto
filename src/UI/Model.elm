module UI.Model exposing
    ( ButtonKind(..)
    , DevView(..)
    , UI
    , ZoomLevel(..)
    , clearConfirmation
    , initialModel
    , requestConfirmation
    )

import Html.Events.Extra.Pointer as Pointer
import Tilemap.Cell exposing (Cell)
import UI.Editor


type alias UI =
    { showMenu : Bool
    , zoomLevel : ZoomLevel
    , showDevMenu : Bool
    , selectedDevView : DevView
    , editor : UI.Editor.Model
    , showLmInfo : Bool
    , pendingDestructiveCell : Maybe Cell
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
    | ConfirmDestructive
    | CancelDestructive


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
    , pendingDestructiveCell = Nothing
    , lastEventDevice = Pointer.MouseType
    }


{-| Show the destructive-placement confirmation for `cell`, closing any other
open panel so the dialog has exclusive focus (same mutual-exclusion pattern as
the menu and LmInfo toggles).
-}
requestConfirmation : Cell -> UI -> UI
requestConfirmation cell ui =
    { ui
        | pendingDestructiveCell = Just cell
        , showMenu = False
        , showLmInfo = False
    }


clearConfirmation : UI -> UI
clearConfirmation ui =
    { ui | pendingDestructiveCell = Nothing }
