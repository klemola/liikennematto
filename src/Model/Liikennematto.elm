module Model.Liikennematto exposing
    ( DrivenWFC(..)
    , GameAction(..)
    , GameFSM
    , GameState(..)
    , GameUpdateContext
    , InitSteps
    , Liikennematto
    , currentState
    , fromNewGame
    , fromPreviousGame
    , initial
    , triggerLoading
    , withTilemap
    )

import Data.Assets exposing (roads)
import Duration exposing (Duration)
import Lib.FSM as FSM exposing (FSM)
import Message exposing (Message)
import Model.Debug exposing (DebugState, initialDebugState, toggleDebugPanel)
import Model.Flags exposing (Flags, RuntimeEnvironment(..))
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Simulation.Car exposing (CarState(..))
import Tilemap.Core exposing (TileListFilter(..), Tilemap)
import Tilemap.WFC as WFC
import Time
import UI.Editor
import UI.ZoomControl


type alias Liikennematto =
    { game : GameFSM
    , initSteps : InitSteps
    , screen : Screen
    , time : Time.Posix
    , world : World
    , wfc : DrivenWFC
    , previousWorld : Maybe World
    , simulationActive : Bool
    , renderCache : RenderCache Message
    , dynamicTiles : RenderCache.DynamicTilesPresentation
    , debug : DebugState
    , errorMessage : Maybe String
    , editor : UI.Editor.Model
    , zoomControl : UI.ZoomControl.Model
    }


type DrivenWFC
    = WFCPending
    | WFCSolved
    | WFCActive WFC.Model


type alias InitSteps =
    { audioInitComplete : Bool
    , viewportSizeSet : Bool
    }



--
-- Game level FSM
--


type alias GameFSM =
    FSM GameState GameAction GameUpdateContext


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


horizontalCellsAmount : Int
horizontalCellsAmount =
    16


verticalCellsAmount : Int
verticalCellsAmount =
    12


tilemapConfig =
    { horizontalCellsAmount = horizontalCellsAmount
    , verticalCellsAmount = verticalCellsAmount
    }


debugState =
    initialDebugState
        |> toggleDebugPanel
        |> Model.Debug.toggleLayer Model.Debug.WFCDebug


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
    , wfc = WFCPending
    , simulationActive = True
    , renderCache = RenderCache.new initialWorld roads
    , dynamicTiles = []
    , debug = debugState
    , errorMessage = Nothing
    , editor = UI.Editor.initialModel
    , zoomControl = UI.ZoomControl.initialModel
    }



--
-- Synchronized changes to multiple root level fields
--


withTilemap : Tilemap -> DrivenWFC -> Liikennematto -> Liikennematto
withTilemap tilemap drivenWFC model =
    let
        nextWorld =
            World.setTilemap tilemap model.world

        renderCacheWFC =
            case drivenWFC of
                WFCActive wfc ->
                    Just wfc

                _ ->
                    Nothing
    in
    { model
        | world = nextWorld
        , wfc = drivenWFC
        , renderCache = RenderCache.setTilemapCache nextWorld.tilemap renderCacheWFC model.renderCache
    }



--
-- Start or restore a game
--


fromNewGame : Maybe World -> Liikennematto -> Liikennematto
fromNewGame previousWorld model =
    { model
        | world = initialWorld
        , wfc = WFCPending
        , previousWorld = previousWorld
        , renderCache = RenderCache.new initialWorld roads
        , simulationActive = True
        , debug = debugState
    }


fromPreviousGame : Liikennematto -> Liikennematto
fromPreviousGame model =
    case model.previousWorld of
        Just previousWorld ->
            { model
                | world = previousWorld
                , wfc = WFCPending
                , previousWorld = Nothing
                , renderCache = RenderCache.new previousWorld roads
                , simulationActive = True
                , debug = debugState
            }

        Nothing ->
            fromNewGame Nothing model
