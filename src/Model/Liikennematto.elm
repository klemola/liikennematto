module Model.Liikennematto exposing
    ( GameAction(..)
    , GameFSM
    , GameState(..)
    , GameUpdateContext
    , InitSteps
    , Liikennematto
    , SimulationState(..)
    , currentState
    , fromNewGame
    , fromPreviousGame
    , initial
    , triggerLoading
    )

import Data.Assets exposing (roadsLegacy)
import Data.Defaults exposing (horizontalCellsAmount, verticalCellsAmount)
import Data.TileSet exposing (allTiles, defaultTile)
import Duration exposing (Duration)
import FSM exposing (FSM)
import Model.Debug exposing (DebugState, initialDebugState)
import Model.Editor as Editor exposing (Editor)
import Model.Flags exposing (Flags, RuntimeEnvironment(..))
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Random
import Time


type alias Liikennematto =
    { game : GameFSM
    , initSteps : InitSteps
    , screen : Screen
    , time : Time.Posix
    , world : World
    , previousWorld : Maybe World
    , simulation : SimulationState
    , editor : Editor
    , renderCache : RenderCache
    , dynamicTiles : RenderCache.DynamicTilesPresentation
    , debug : DebugState
    , errorMessage : Maybe String
    }


type alias GameFSM =
    FSM GameState GameAction GameUpdateContext


type SimulationState
    = Running
    | Paused


type alias InitSteps =
    { audioInitComplete : Bool
    , viewportSizeSet : Bool
    }



--
-- Game level FSM
--


type GameState
    = WaitingForInit
    | InSplashScreen
    | Loading
    | InGame
    | Error


type GameAction
    = StartIntro
    | TriggerPostLoadingEffects
    | TriggerInGameEffects


type alias GameUpdateContext =
    InitSteps


splashScreenTimer : Duration
splashScreenTimer =
    Duration.milliseconds 1500


loadingTimer : Duration
loadingTimer =
    Duration.milliseconds 1000


waitingForInit : FSM.State GameState GameAction GameUpdateContext
waitingForInit =
    FSM.createState
        { id = FSM.createStateId "game-waiting-for-init"
        , kind = WaitingForInit
        , transitions =
            [ FSM.createTransition
                (\_ -> inSplashScreen)
                []
                (FSM.Condition (\initSteps _ -> initSteps.audioInitComplete && initSteps.viewportSizeSet))
            ]
        , entryActions = []
        , exitActions = []
        }


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
        , entryActions = [ TriggerInGameEffects ]
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


tilemapConfig =
    { horizontalCellsAmount = horizontalCellsAmount
    , verticalCellsAmount = verticalCellsAmount
    , initialSeed = Random.initialSeed 0
    , defaultTile = defaultTile
    , tiles = allTiles
    }


initialWorld : World
initialWorld =
    World.empty tilemapConfig


initial : Flags -> Liikennematto
initial flags =
    let
        ( appFsm, _ ) =
            FSM.initialize waitingForInit

        skipExternalInit =
            flags.runtimeEnvironment == Unknown
    in
    { game = appFsm
    , initSteps =
        { audioInitComplete = skipExternalInit
        , viewportSizeSet = False
        }
    , screen = Screen.fallback
    , time = Time.millisToPosix 0
    , previousWorld = Nothing
    , world = initialWorld
    , simulation = Running
    , editor = Editor.initial
    , renderCache = RenderCache.new initialWorld roadsLegacy
    , dynamicTiles = []
    , debug = initialDebugState
    , errorMessage = Nothing
    }



--
-- Start or restore a game
--


fromNewGame : Maybe World -> Liikennematto -> Liikennematto
fromNewGame previousWorld model =
    { model
        | world = initialWorld
        , previousWorld = previousWorld
        , editor = Editor.reset model.editor
        , renderCache = RenderCache.new initialWorld roadsLegacy
        , simulation = Paused
        , debug = initialDebugState
    }


fromPreviousGame : Liikennematto -> Liikennematto
fromPreviousGame model =
    case model.previousWorld of
        Just previousWorld ->
            { model
                | world = previousWorld
                , previousWorld = Nothing
                , editor = Editor.reset model.editor
                , renderCache = RenderCache.new previousWorld roadsLegacy
                , simulation = Paused
                , debug = initialDebugState
            }

        Nothing ->
            fromNewGame Nothing model
