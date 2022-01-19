module Model.Liikennematto exposing
    ( CarSpawnQueue
    , Liikennematto
    , SimulationState(..)
    , Tool(..)
    , new
    )

import Data.Worlds exposing (defaultWorld)
import Element
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.World exposing (World)
import Random


type alias Liikennematto =
    { world : World
    , screen : Screen
    , seed : Random.Seed
    , simulation : SimulationState
    , renderCache : RenderCache
    , tool : Tool
    , carSpawnQueue : CarSpawnQueue
    , showDebugPanel : Bool
    , showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    , roadNetworkDotString : Maybe String
    }


type SimulationState
    = Running
    | Paused


type alias CarSpawnQueue =
    Int


type alias Screen =
    { width : Int
    , height : Int
    , orientation : Element.Orientation
    }


type Tool
    = SmartConstruction
    | Bulldozer
    | Dynamite
    | None


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed 666


new : Liikennematto
new =
    { world = defaultWorld
    , screen =
        { width = 375
        , height = 667
        , orientation = Element.Landscape
        }
    , seed = initialSeed
    , simulation = Running
    , renderCache = RenderCache.new defaultWorld
    , tool = SmartConstruction
    , carSpawnQueue = 0
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }
