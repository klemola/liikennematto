module Model.Editor exposing
    ( CarSpawnQueue
    , Editor
    , Tool(..)
    , activateTool
    , maxQueuedCars
    , new
    , setCarSpawnQueue
    )


type alias Editor =
    { tool : Tool
    , carSpawnQueue : CarSpawnQueue
    }


type Tool
    = SmartConstruction
    | Bulldozer
    | Dynamite
    | None


type alias CarSpawnQueue =
    Int


maxQueuedCars : Int
maxQueuedCars =
    5


new : Editor
new =
    { tool = SmartConstruction
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
