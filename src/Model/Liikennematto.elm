module Model.Liikennematto exposing
    ( Liikennematto
    , SimulationState(..)
    , fromNewGame
    , fromPreviousGame
    , initial
    )

import Data.Defaults exposing (horizontalCellsAmount, verticalCellsAmount)
import Model.Editor as Editor exposing (Editor)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Random


type alias Liikennematto =
    { world : World
    , previousWorld : Maybe World
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


initialWorld : World
initialWorld =
    World.empty
        { horizontalCellsAmount = horizontalCellsAmount
        , verticalCellsAmount = verticalCellsAmount
        }


initial : Liikennematto
initial =
    { world = initialWorld
    , previousWorld = Nothing
    , screen = Screen.fallback
    , seed = initialSeed
    , simulation = Running
    , editor = Editor.initial
    , renderCache = RenderCache.new initialWorld
    , dynamicTiles = []
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    }


fromNewGame : Maybe World -> Liikennematto -> Liikennematto
fromNewGame previousWorld model =
    let
        nextWorld =
            World.empty
                { horizontalCellsAmount = Data.Defaults.horizontalCellsAmount
                , verticalCellsAmount = Data.Defaults.verticalCellsAmount
                }

        baseEditor =
            Editor.initial
    in
    { model
        | editor =
            { baseEditor
                | lastEventDevice = model.editor.lastEventDevice
                , zoomLevel = model.editor.zoomLevel
            }
        , world = nextWorld
        , previousWorld = previousWorld
        , renderCache = RenderCache.new nextWorld
        , simulation = Running
    }


fromPreviousGame : Liikennematto -> Liikennematto
fromPreviousGame model =
    case model.previousWorld of
        Just previousWorld ->
            { model
                | world = previousWorld
                , previousWorld = Nothing
                , editor = Editor.reset model.editor
                , renderCache = RenderCache.new previousWorld
            }

        Nothing ->
            fromNewGame Nothing model
