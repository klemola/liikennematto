module Model.Editor exposing
    ( CarSpawnQueue
    , Editor
    , PendingTilemapChange
    , ZoomLevel(..)
    , activateCell
    , advanceLongPressTimer
    , clearPointerDownEvent
    , combineChangedCells
    , createPendingTilemapChange
    , deactivateCell
    , hasPendingTilemapChange
    , maxQueuedCars
    , minTilemapChangeFrequency
    , mouseDetected
    , new
    , resetLongPressTimer
    , selectCell
    , setCarSpawnQueue
    , setLastEventDevice
    , setZoomLevel
    , zoomLevelToUIValue
    )

import Duration exposing (Duration)
import Html.Events.Extra.Pointer as Pointer
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Quantity
import Set exposing (Set)


type alias Editor =
    { zoomLevel : ZoomLevel
    , pendingTilemapChange : Maybe PendingTilemapChange
    , carSpawnQueue : CarSpawnQueue
    , activeCell : Maybe Cell
    , longPressTimer : Maybe Duration
    , pointerDownEvent : Maybe Pointer.Event
    , lastEventDevice : Pointer.DeviceType
    }


type ZoomLevel
    = Near
    | Far
    | VeryFar


type alias PendingTilemapChange =
    ( Duration, Set CellCoordinates )


type alias CarSpawnQueue =
    Int


minTilemapChangeFrequency : Duration
minTilemapChangeFrequency =
    Duration.milliseconds 750


maxQueuedCars : Int
maxQueuedCars =
    5


new : Editor
new =
    { zoomLevel = Far
    , pendingTilemapChange = Nothing
    , carSpawnQueue = 0
    , activeCell = Nothing
    , longPressTimer = Nothing
    , pointerDownEvent = Nothing
    , lastEventDevice = Pointer.MouseType
    }


setZoomLevel : Float -> Editor -> Editor
setZoomLevel level editor =
    { editor | zoomLevel = uiValueToZoomLevel level }


uiValueToZoomLevel : Float -> ZoomLevel
uiValueToZoomLevel value =
    case floor value of
        1 ->
            VeryFar

        2 ->
            Far

        3 ->
            Near

        _ ->
            Far


zoomLevelToUIValue : ZoomLevel -> Float
zoomLevelToUIValue zoomLevel =
    case zoomLevel of
        VeryFar ->
            1

        Far ->
            2

        Near ->
            3


setCarSpawnQueue : Int -> Editor -> Editor
setCarSpawnQueue queue editor =
    if queue > maxQueuedCars then
        editor

    else
        { editor | carSpawnQueue = queue }


createPendingTilemapChange : List Cell -> Editor -> Editor
createPendingTilemapChange changedCells editor =
    let
        pendingTilemapChange =
            Just
                ( minTilemapChangeFrequency
                , combineChangedCells changedCells Set.empty
                )
    in
    { editor | pendingTilemapChange = pendingTilemapChange }


combineChangedCells : List Cell -> Set CellCoordinates -> Set CellCoordinates
combineChangedCells changedCells currentChanges =
    changedCells
        |> List.map Cell.coordinates
        |> Set.fromList
        |> Set.union currentChanges


hasPendingTilemapChange : Editor -> Bool
hasPendingTilemapChange editor =
    editor.pendingTilemapChange /= Nothing


activateCell : Cell -> Editor -> Editor
activateCell cell editor =
    if
        -- Cell already active?
        editor.activeCell
            |> Maybe.map (Cell.identical cell)
            |> Maybe.withDefault False
    then
        editor

    else
        { editor | activeCell = Just cell }


deactivateCell : Editor -> Editor
deactivateCell editor =
    { editor | activeCell = Nothing }


storePointerDownEvent : Pointer.Event -> Editor -> Editor
storePointerDownEvent event editor =
    { editor | pointerDownEvent = Just event }


clearPointerDownEvent : Editor -> Editor
clearPointerDownEvent editor =
    { editor | pointerDownEvent = Nothing }


resetLongPressTimer : Editor -> Editor
resetLongPressTimer editor =
    { editor | longPressTimer = Nothing }


advanceLongPressTimer : Duration -> Editor -> Editor
advanceLongPressTimer delta editor =
    { editor
        | longPressTimer = editor.longPressTimer |> Maybe.map (Quantity.plus delta)
    }


setLastEventDevice : Pointer.DeviceType -> Editor -> Editor
setLastEventDevice deviceType editor =
    { editor | lastEventDevice = deviceType }


mouseDetected : Editor -> Bool
mouseDetected editor =
    editor.lastEventDevice == Pointer.MouseType


selectCell : Pointer.Event -> Cell -> Bool -> Editor -> Editor
selectCell event eventCell hasTile initialEditor =
    initialEditor
        |> setLastEventDevice event.pointerType
        |> storePointerDownEvent event
        |> activateCell eventCell
        |> (\editor ->
                if event.pointerType == Pointer.MouseType || not hasTile then
                    editor

                else
                    { editor | longPressTimer = Just Quantity.zero }
           )
