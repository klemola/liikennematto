module Model.Liikennematto exposing
    ( GameAction(..)
    , GameFSM
    , GameState(..)
    , GameUpdateContext
    , Liikennematto
    , SimulationState(..)
    , currentState
    , fromNewGame
    , fromPreviousGame
    , initial
    , triggerLoading
    )

import Data.Defaults exposing (horizontalCellsAmount, verticalCellsAmount)
import Duration exposing (Duration)
import FSM exposing (FSM)
import Model.Editor as Editor exposing (Editor)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Random


type alias Liikennematto =
    { game : GameFSM
    , screen : Screen
    , seed : Random.Seed
    , world : World
    , previousWorld : Maybe World
    , simulation : SimulationState
    , editor : Editor
    , renderCache : RenderCache
    , dynamicTiles : RenderCache.DynamicTilesPresentation
    , showDebugPanel : Bool
    , showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    , roadNetworkDotString : Maybe String
    , errorMessage : Maybe String
    }


type alias GameFSM =
    FSM GameState GameAction GameUpdateContext


type SimulationState
    = Running
    | Paused



--
-- Game level FSM
--


type GameState
    = InSplashScreen
    | Loading
    | InGame
    | Error


type GameAction
    = StartIntro
    | TriggerPostLoadingEffects


type alias GameUpdateContext =
    ()


splashScreenTimer : Duration
splashScreenTimer =
    Duration.milliseconds 1500


loadingTimer : Duration
loadingTimer =
    Duration.milliseconds 1000


inSplashScreen : FSM.State GameState GameAction GameUpdateContext
inSplashScreen =
    FSM.createState
        { id = FSM.createStateId "game-in-splash-screen"
        , kind = InSplashScreen
        , transitions =
            [ FSM.createTransition
                (\_ -> inGame)
                []
                (FSM.Timer splashScreenTimer)
            ]
        , entryActions = []
        , exitActions = [ StartIntro ]
        }


loading : FSM.State GameState GameAction GameUpdateContext
loading =
    FSM.createState
        { id = FSM.createStateId "game-loading"
        , kind = Loading
        , transitions =
            [ FSM.createTransition
                (\_ -> inGame)
                []
                (FSM.Timer loadingTimer)
            ]
        , entryActions = []
        , exitActions = [ TriggerPostLoadingEffects ]
        }


inGame : FSM.State GameState GameAction GameUpdateContext
inGame =
    FSM.createState
        { id = FSM.createStateId "game-ingame"
        , kind = InGame
        , transitions =
            [ FSM.createTransition
                (\_ -> loading)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> error)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


error : FSM.State GameState GameAction GameUpdateContext
error =
    FSM.createState
        { id = FSM.createStateId "game-error"
        , kind = Error
        , transitions =
            [ FSM.createTransition
                (\_ -> inSplashScreen)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


currentState : Liikennematto -> GameState
currentState model =
    FSM.toCurrentState model.game


triggerLoading : Liikennematto -> ( Liikennematto, List GameAction )
triggerLoading model =
    case FSM.transitionTo (FSM.getId loading) model.game of
        Ok ( nextFsm, actions ) ->
            ( { model | game = nextFsm }, actions )

        Err message ->
            let
                ( nextFsm, actions ) =
                    FSM.initialize error
            in
            ( { model
                | game = nextFsm
                , errorMessage = Just message
              }
            , actions
            )



--
-- Init
--


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
    let
        ( appFsm, _ ) =
            FSM.initialize inSplashScreen
    in
    { game = appFsm
    , screen = Screen.fallback
    , seed = initialSeed
    , previousWorld = Nothing
    , world = initialWorld
    , simulation = Running
    , editor = Editor.initial
    , renderCache = RenderCache.new initialWorld
    , dynamicTiles = []
    , showDebugPanel = False
    , showRoadNetwork = False
    , showCarDebugVisuals = False
    , roadNetworkDotString = Nothing
    , errorMessage = Nothing
    }



--
-- Start or restore a game
--


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
        | world = nextWorld
        , previousWorld = previousWorld
        , editor =
            { baseEditor
                | lastEventDevice = model.editor.lastEventDevice
                , zoomLevel = model.editor.zoomLevel
            }
        , renderCache = RenderCache.new nextWorld
        , simulation = Paused
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
                , simulation = Paused
            }

        Nothing ->
            fromNewGame Nothing model
