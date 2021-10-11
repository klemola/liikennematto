module Model.Liikennematto exposing
    ( CarSpawnQueue
    , Liikennematto
    , SimulationState(..)
    , Tool(..)
    , latestTilemap
    , new
    )

import Element
import Model.ActiveAnimations as Animations exposing (ActiveAnimations)
import Model.Tilemap exposing (Tilemap, TilemapChange)
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
    , pendingTilemapChange : Maybe TilemapChange
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
    , pendingTilemapChange = Nothing
    , animations = Animations.empty
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }


latestTilemap : Liikennematto -> Tilemap
latestTilemap model =
    model.pendingTilemapChange
        |> Maybe.map .nextTilemap
        |> Maybe.withDefault model.world.tilemap
