module Main exposing (main)

import Audio
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Duration exposing (Duration)
import Element exposing (Element)
import Lib.FSM as FSM
import Message exposing (Message(..))
import Model.Flags as Flags exposing (FlagsJson)
import Model.Liikennematto as Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (setPixelsToMetersRatio, setTilemapCache)
import Model.Screen as Screen
import Render
import Render.Debug
import Simulation.Update as Simulation
import Task
import Tilemap.Update as Tilemap
import Time
import UI.Core exposing (containerId, renderSafeAreaYSize)
import UI.ErrorScreen
import UI.SplashScreen
import UI.UI


main : Program FlagsJson Liikennematto Message
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : FlagsJson -> ( Liikennematto, Cmd Message )
init flags =
    let
        flagsDecoded =
            Flags.fromJsonValue flags

        initialModel =
            Liikennematto.initial flagsDecoded

        initCmds =
            -- simulate a screen resize
            Task.perform WindowResized getViewport
    in
    ( initialModel, initCmds )


environmentUpdateFrequencyMs : Float
environmentUpdateFrequencyMs =
    1000


secondarySystemFrequencyMs : Float
secondarySystemFrequencyMs =
    -- 30 FPS/UPS
    1000 / 30


secondarySystemFrequencyDelta : Duration
secondarySystemFrequencyDelta =
    Duration.milliseconds secondarySystemFrequencyMs


subscriptions : Liikennematto -> Sub Message
subscriptions model =
    let
        defaultSubs =
            [ Events.onResize (\_ _ -> ResizeTriggered)
            , Events.onVisibilityChange VisibilityChanged
            , Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
            , Time.every secondarySystemFrequencyMs (always (UpdateTilemap secondarySystemFrequencyDelta))
            , Audio.onAudioInitComplete (\_ -> AudioInitComplete)
            , UI.UI.subscriptions model
            ]
    in
    if not model.simulationActive then
        Sub.batch defaultSubs

    else
        Sub.batch
            (defaultSubs
                ++ [ Events.onAnimationFrameDelta (Duration.milliseconds >> UpdateTraffic)
                   , Time.every environmentUpdateFrequencyMs (always UpdateEnvironment)
                   , Time.every secondarySystemFrequencyMs (\time -> CheckQueues time secondarySystemFrequencyDelta)
                   ]
            )


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    updateBase msg model
        |> withUpdate UI.UI.update msg
        |> withUpdate Tilemap.update msg
        |> withUpdate Simulation.update msg


updateBase : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
updateBase msg model =
    case msg of
        AnimationFrameReceived delta ->
            let
                ( nextGameFSM, gameActions ) =
                    FSM.update delta model.initSteps model.game
            in
            ( { model | game = nextGameFSM }
            , gameActionsToCmd gameActions
            )

        VisibilityChanged newVisibility ->
            ( { model
                | simulationActive =
                    case newVisibility of
                        Events.Visible ->
                            model.simulationActive

                        Events.Hidden ->
                            False
              }
            , Cmd.none
            )

        ResizeTriggered ->
            ( model, Task.perform WindowResized getViewport )

        WindowResized domViewport ->
            let
                { initSteps } =
                    model

                nextScreen =
                    Screen.fromDimensions
                        (round domViewport.viewport.width)
                        (round domViewport.viewport.height)

                nextInitSteps =
                    { initSteps | viewportSizeSet = True }
            in
            ( { model
                | screen = nextScreen
                , initSteps = nextInitSteps
              }
            , Cmd.none
            )

        AudioInitComplete ->
            let
                { initSteps } =
                    model

                nextInitSteps =
                    { initSteps | audioInitComplete = True }
            in
            ( { model | initSteps = nextInitSteps }
            , Cmd.none
            )

        NewGame ->
            let
                previousWorld =
                    Just model.world

                ( modelWithTransition, transitionActions ) =
                    Liikennematto.triggerLoading model
            in
            ( Liikennematto.fromNewGame previousWorld modelWithTransition
            , gameActionsToCmd transitionActions
            )

        RestoreGame ->
            let
                ( modelWithTransition, transitionActions ) =
                    Liikennematto.triggerLoading model
            in
            ( Liikennematto.fromPreviousGame modelWithTransition
            , gameActionsToCmd transitionActions
            )

        ToggleSimulationActive ->
            ( { model | simulationActive = not model.simulationActive }, Cmd.none )

        ZoomLevelChanged nextLevel ->
            let
                nextRenderCache =
                    model.renderCache
                        |> setPixelsToMetersRatio nextLevel
                        |> setTilemapCache model.world.tilemap Nothing
            in
            ( { model | renderCache = nextRenderCache }
            , Browser.Dom.getViewportOf containerId
                |> Task.andThen
                    (\domViewport ->
                        let
                            mapSizeChangeX =
                                nextRenderCache.tilemapWidthPixels - model.renderCache.tilemapWidthPixels

                            mapSizeChangeY =
                                nextRenderCache.tilemapHeightPixels - model.renderCache.tilemapHeightPixels

                            nextScrollX =
                                (mapSizeChangeX / 2) + domViewport.viewport.x

                            nextScrollY =
                                (mapSizeChangeY / 2) + domViewport.viewport.y
                        in
                        Browser.Dom.setViewportOf containerId (max nextScrollX 0) (max nextScrollY 0)
                    )
                |> Task.attempt (\_ -> NoOp)
            )

        _ ->
            ( model, Cmd.none )


withUpdate :
    (Message -> Liikennematto -> ( Liikennematto, Cmd Message ))
    -> Message
    -> ( Liikennematto, Cmd Message )
    -> ( Liikennematto, Cmd Message )
withUpdate updateFn msg ( model, cmds ) =
    let
        ( nextModel, cmd ) =
            updateFn msg model
    in
    ( nextModel, Cmd.batch [ cmds, cmd ] )


gameActionsToCmd : List Liikennematto.GameAction -> Cmd Message
gameActionsToCmd actions =
    if List.isEmpty actions then
        Cmd.none

    else
        actions
            |> List.map gameActionToCmd
            |> Cmd.batch


gameActionToCmd : Liikennematto.GameAction -> Cmd Message
gameActionToCmd action =
    case action of
        Liikennematto.StartIntro ->
            Message.asCmd GameSetupComplete

        Liikennematto.TriggerPostLoadingEffects ->
            Message.asCmd ToggleSimulationActive

        Liikennematto.TriggerInGameEffects ->
            Browser.Dom.getViewportOf containerId
                |> Task.andThen
                    (\domViewport ->
                        Browser.Dom.setViewportOf
                            containerId
                            ((domViewport.scene.width - domViewport.viewport.width) / 2)
                            ((domViewport.scene.height - domViewport.viewport.height - toFloat renderSafeAreaYSize) / 2)
                    )
                |> Task.attempt (\_ -> NoOp)


view : Liikennematto -> Browser.Document Message
view model =
    { title = "Liikennematto"
    , body =
        [ case Liikennematto.currentState model of
            Liikennematto.InGame ->
                UI.UI.view
                    model
                    (render model)
                    (renderDebug model)

            Liikennematto.Error ->
                UI.ErrorScreen.view model.errorMessage

            -- Show the splash screen for both the game init and loading states
            _ ->
                UI.SplashScreen.view model.screen
        ]
    }


render : Liikennematto -> Element Message
render model =
    Render.view model.world model.renderCache
        |> Element.html
        |> Element.map (always NoOp)


renderDebug : Liikennematto -> Element Message
renderDebug model =
    Render.Debug.view model.world model.renderCache model.debug
        |> Element.html
        |> Element.map (always NoOp)
