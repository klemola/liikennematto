module UI.Model exposing
    ( ButtonKind(..)
    , DestructiveAction(..)
    , DevView(..)
    , UI
    , ZoomLevel(..)
    , clearConfirmation
    , destructiveActionCell
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
    , pendingDestructiveAction : Maybe DestructiveAction
    , lastEventDevice : Pointer.DeviceType
    }


{-| A tilemap change that would erase a lot or large nature tile (no undo),
awaiting player confirmation.
-}
type DestructiveAction
    = DestructiveRoadPlacement Cell
    | DestructiveTileRemoval Cell


destructiveActionCell : DestructiveAction -> Cell
destructiveActionCell action =
    case action of
        DestructiveRoadPlacement cell ->
            cell

        DestructiveTileRemoval cell ->
            cell


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
    , pendingDestructiveAction = Nothing
    , lastEventDevice = Pointer.MouseType
    }


{-| Show the destructive-change confirmation for `action`, closing any other
open panel so the dialog has exclusive focus (same mutual-exclusion pattern as
the menu and LmInfo toggles).
-}
requestConfirmation : DestructiveAction -> UI -> UI
requestConfirmation action ui =
    { ui
        | pendingDestructiveAction = Just action
        , showMenu = False
        , showLmInfo = False
    }


clearConfirmation : UI -> UI
clearConfirmation ui =
    { ui | pendingDestructiveAction = Nothing }
