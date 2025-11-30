module Model.Liikennematto exposing
    ( GameAction(..)
    , GameFSM
    , GameState(..)
    , GameUpdateContext
    , InitSteps
    , Liikennematto
    , currentState
    , fromNewGame
    , initial
    , triggerLoading
    )

import Duration exposing (Duration)
import Json.Encode as JE
import Lib.FSM as FSM exposing (FSM)
import Lib.SeedState as SeedState
import Model.Debug exposing (DebugState, initialDebugState)
import Model.Flags exposing (Flags, RuntimeEnvironment(..))
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.Screen as Screen exposing (Screen)
import Model.World as World exposing (World)
import Random
import Render.Viewport as Viewport exposing (Viewport)
import Savegame
import Tilemap.DrivenWFC exposing (DrivenWFC(..))
import Time
import UI.Model exposing (UI)


type alias Liikennematto =
    { game : GameFSM
    , initSteps : InitSteps
    , screen : Screen
    , time : Time.Posix
    , world : World
    , wfc : DrivenWFC
    , savegame : Maybe JE.Value
    , simulationActive : Bool
    , renderCache : RenderCache
    , viewport : Viewport
    , debug : DebugState
    , errorMessage : Maybe String
    , ui : UI
    }


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


initialDrivenWfc : Random.Seed -> DrivenWFC
initialDrivenWfc initialSeed =
    WFCSolved [] [] (SeedState.fromSeed initialSeed)


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


initialWorld : Random.Seed -> World
initialWorld initialSeed =
    World.empty initialSeed tilemapConfig


initial : Flags -> Liikennematto
initial flags =
    let
        ( appFsm, _ ) =
            FSM.initialize waitingForInit

        skipExternalInit =
            flags.runtimeEnvironment == Unknown

        initialSeed =
            Random.initialSeed (Time.posixToMillis flags.time)

        ( world, maybeSavegame ) =
            case flags.savegameData of
                Just savegameJson ->
                    case Savegame.decode savegameJson of
                        Ok restoredWorld ->
                            ( Savegame.spawnLotResidents flags.time restoredWorld
                            , Just savegameJson
                            )

                        Err _ ->
                            ( initialWorld initialSeed, Nothing )

                Nothing ->
                    ( initialWorld initialSeed, Nothing )
    in
    let
        initialRenderCache =
            RenderCache.new world

        initialViewport =
            Viewport.init
                { tilemapWidth = initialRenderCache.tilemapWidthPixels
                , tilemapHeight = initialRenderCache.tilemapHeightPixels
                , viewportWidth = 720.0
                , viewportHeight = 476.0
                , pixelsToMetersRatio = initialRenderCache.pixelsToMetersRatio
                }
    in
    { game = appFsm
    , initSteps =
        { audioInitComplete = skipExternalInit
        , viewportSizeSet = False
        }
    , screen = Screen.fallback
    , time = Time.millisToPosix 0
    , world = world
    , wfc = initialDrivenWfc initialSeed
    , savegame = maybeSavegame
    , simulationActive = True
    , renderCache = initialRenderCache
    , viewport = initialViewport
    , debug = initialDebugState
    , errorMessage = Nothing
    , ui = UI.Model.initialModel
    }



--
-- Start or restore a game
--


fromNewGame : Liikennematto -> Liikennematto
fromNewGame model =
    let
        ( randomInt, _ ) =
            Random.step
                (Random.int Random.minInt Random.maxInt)
                (World.currentSeed model.world)

        newGameSeed =
            Random.initialSeed randomInt

        world =
            initialWorld newGameSeed
    in
    { model
        | world = world
        , wfc = initialDrivenWfc newGameSeed
        , renderCache = RenderCache.new world
        , simulationActive = True
        , debug = initialDebugState
    }
