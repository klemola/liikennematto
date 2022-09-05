module Model.Liikennematto exposing
    ( Liikennematto
    , Screen
    , SimulationState(..)
    , new
    )

import Element
import Model.Editor as Editor exposing (Editor)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.World exposing (World)
import Random


type alias Liikennematto =
    { world : World
    , screen : Screen
    , seed : Random.Seed
    , simulation : SimulationState
    , editor : Editor
    , renderCache : RenderCache
    , showDebugPanel : Bool
    , showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    , roadNetworkDotString : Maybe String
    }


type SimulationState
    = Running
    | Paused


type alias Screen =
    { width : Int
    , height : Int
    , orientation : Element.Orientation
    }


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed 666


new : World -> Liikennematto
new world =
    { world = world
    , screen =
        { width = 375
        , height = 667
        , orientation = Element.Landscape
        }
    , seed = initialSeed
    , simulation = Running
    , editor = Editor.new
    , renderCache = RenderCache.new world
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }
