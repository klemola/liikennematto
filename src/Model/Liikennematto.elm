module Model.Liikennematto exposing
    ( Liikennematto
    , SimulationState(..)
    , new
    )

import Data.Defaults exposing (horizontalCellsAmount, verticalCellsAmount)
import Model.Editor as Editor exposing (Editor)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Random


type alias Liikennematto =
    { world : World
    , screen : Screen
    , seed : Random.Seed
    , simulation : SimulationState
    , editor : Editor
    , renderCache : RenderCache
    , dynamicTiles : RenderCache.DynamicTilesPresentation
    , showDebugPanel : Bool
    , showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    , roadNetworkDotString : Maybe String
    }


type SimulationState
    = Running
    | Paused


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed 666


initialWorld =
    World.empty
        { horizontalCellsAmount = horizontalCellsAmount
        , verticalCellsAmount = verticalCellsAmount
        }


new : Liikennematto
new =
    { world = initialWorld
    , screen = Screen.fallback
    , seed = initialSeed
    , simulation = Running
    , editor = Editor.new
    , renderCache = RenderCache.new initialWorld
    , dynamicTiles = []
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }
