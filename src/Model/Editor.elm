module Model.Editor exposing
    ( CarSpawnQueue
    , Editor
    , PendingTilemapChange
    , Tool(..)
    , ZoomLevel(..)
    , activateTool
    , combineChangedCells
    , createPendingTilemapChange
    , hasPendingTilemapChange
    , maxQueuedCars
    , minTilemapChangeFrequency
    , new
    , setCarSpawnQueue
    , setZoomLevel
    , zoomLevelToUIValue
    )

import Duration exposing (Duration)
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Set exposing (Set)


type alias Editor =
    { tool : Tool
    , zoomLevel : ZoomLevel
    , pendingTilemapChange : Maybe PendingTilemapChange
    , carSpawnQueue : CarSpawnQueue
    }


type Tool
    = SmartConstruction
    | Bulldozer
    | Dynamite


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
    { tool = SmartConstruction
    , zoomLevel = Far
    , pendingTilemapChange = Nothing
    , carSpawnQueue = 0
    }


activateTool : Tool -> Editor -> Editor
activateTool tool editor =
    { editor | tool = tool }


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
