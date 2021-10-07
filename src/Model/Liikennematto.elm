module Model.Liikennematto exposing
    ( CarSpawnQueue
    , Liikennematto
    , SimulationState(..)
    , Tool(..)
    , new
    )

import Element
import Model.ActiveAnimations as Animations exposing (ActiveAnimations)
import Model.World exposing (World)
import Random
import Worlds exposing (defaultWorld)


type alias Liikennematto =
    { world : World
    , screen : Screen
    , seed : Random.Seed
    , simulation : SimulationState
    , tool : Tool
    , carSpawnQueue : CarSpawnQueue
    , animations : ActiveAnimations
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
    , tool = SmartConstruction
    , carSpawnQueue = 0
    , animations = Animations.empty
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }
