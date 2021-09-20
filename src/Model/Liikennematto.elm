module Model.Liikennematto exposing
    ( CarSpawnQueue
    , Liikennematto
    , SimulationState(..)
    , Tool(..)
    , new
    )

import Config
import Defaults
import Element
import Length
import Model.Board as Board
import Model.Car exposing (Car)
import Model.Geometry exposing (LMEntityCoordinates)
import Model.World exposing (World)
import QuadTree exposing (QuadTree)
import Random


type alias Liikennematto =
    { world : World
    , screen : Screen
    , seed : Random.Seed
    , simulation : SimulationState
    , carSpawnQueue : CarSpawnQueue
    , carPositionLookup : QuadTree Length.Meters LMEntityCoordinates Car
    , tool : Tool
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
    let
        carPositionLookup =
            QuadTree.init Board.boundingBox Config.quadTreeLeafElementsAmount
    in
    { world = Defaults.defaultWorld
    , screen =
        { width = 375
        , height = 667
        , orientation = Element.Landscape
        }
    , seed = initialSeed
    , simulation = Running
    , carSpawnQueue = 0
    , carPositionLookup = carPositionLookup
    , tool = SmartConstruction
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }
