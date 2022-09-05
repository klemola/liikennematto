module Model.Editor exposing
    ( CarSpawnQueue
    , Editor
    , PendingTilemapChange
    , Tool(..)
    , activateTool
    , combineChangedCells
    , createPendingTilemapChange
    , hasPendingTilemapChange
    , maxQueuedCars
    , minTilemapChangeFrequency
    , new
    , setCarSpawnQueue
    )

import Duration exposing (Duration)
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Set exposing (Set)


type alias Editor =
    { tool : Tool
    , pendingTilemapChange : Maybe PendingTilemapChange
    , carSpawnQueue : CarSpawnQueue
    }


type Tool
    = SmartConstruction
    | Bulldozer
    | Dynamite
    | None


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
    , pendingTilemapChange = Nothing
    , carSpawnQueue = 0
    }


activateTool : Tool -> Editor -> Editor
activateTool tool editor =
    { editor | tool = tool }


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
