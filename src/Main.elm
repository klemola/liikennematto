module Main exposing (main)

import Audio
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Duration exposing (Duration)
import Element exposing (Element)
import Html
import Json.Decode as Decode
import Length
import Lib.FSM as FSM
import Message exposing (Message(..))
import Model.Debug
import Model.Flags as Flags exposing (FlagsJson)
import Model.Liikennematto as Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (RenderCache, setTilemapCache)
import Model.Screen as Screen
import Pixels
import Quantity
import Render
import Render.Conversion exposing (toPixelsValue)
import Render.Debug
import Render.Viewport as Viewport
import Savegame
import Simulation.Update as Simulation
import Task
import Tilemap.Update as Tilemap
import Time
import UI
import UI.Core exposing (containerId)
import UI.Editor as Editor
import UI.ErrorScreen
import UI.Model
import UI.SplashScreen


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


keyReleasedDecoder : Decode.Decoder Message
keyReleasedDecoder =
    Decode.map KeyReleased (Decode.field "key" Decode.string)


subscriptions : Liikennematto -> Sub Message
subscriptions model =
    let
        defaultSubs =
            [ Events.onResize (\_ _ -> ResizeTriggered)
            , Events.onVisibilityChange VisibilityChanged
            , Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
            , Events.onKeyUp keyReleasedDecoder
            , Time.every secondarySystemFrequencyMs (always (UpdateTilemap secondarySystemFrequencyDelta))
            , Audio.onAudioInitComplete (\_ -> AudioInitComplete)
            , Savegame.onHashChange SavegameHashChanged
            , Savegame.onHashCleared (\_ -> SavegameHashCleared)
            , UI.subscriptions model.ui |> Sub.map UIMsg
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
                            True

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

                nextViewport =
                    updateViewportSize
                        (round domViewport.viewport.width)
                        (round domViewport.viewport.height)
                        model

                ui =
                    model.ui

                nextEditor =
                    Editor.onViewportChanged model.renderCache nextViewport ui.editor

                nextUi =
                    { ui | editor = nextEditor }
            in
            ( { model
                | screen = nextScreen
                , initSteps = nextInitSteps
                , viewport = nextViewport
                , ui = nextUi
              }
            , Cmd.none
            )

        KeyReleased key ->
            let
                ( nextUi, uiMsg ) =
                    UI.onKeyPress key model.ui
            in
            ( { model | ui = nextUi }
            , Cmd.map UIMsg uiMsg
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

        SavegameHashChanged savegameJson ->
            case Savegame.decode savegameJson of
                Ok restoredWorld ->
                    let
                        worldWithResidents =
                            Savegame.spawnLotResidents model.time restoredWorld
                    in
                    ( { model
                        | world = worldWithResidents
                        , savegame = Just savegameJson
                        , renderCache = Model.RenderCache.new worldWithResidents
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        SavegameHashCleared ->
            ( Liikennematto.fromNewGame model
            , Cmd.none
            )

        UIMsg uiMsg ->
            let
                ( nextUi, uiCmd, uiEvent ) =
                    UI.update model.world model.renderCache.pixelsToMetersRatio model.viewport uiMsg model.ui

                ( modelWithUiEvent, uiEventCmd ) =
                    case uiEvent of
                        Just event ->
                            case event of
                                UI.GameInputReceived inputEvent ->
                                    case inputEvent.kind of
                                        UI.Core.Primary ->
                                            Tilemap.onPrimaryInput inputEvent.cell model

                                        UI.Core.Secondary ->
                                            Tilemap.onSecondaryInput inputEvent.cell model

                                UI.ButtonPressed buttonId ->
                                    onUiButtonPressed buttonId model

                                UI.ZoomLevelChanged zoomLevel ->
                                    onZoomLevelChanged zoomLevel model

                                UI.ViewportChanged deltaX deltaY shouldSnap ->
                                    onViewportChanged deltaX deltaY shouldSnap model

                                UI.DevViewSelected devView ->
                                    ( { model
                                        | debug =
                                            Model.Debug.setLayer Model.Debug.WFCDebug
                                                (devView == UI.Model.WFCOverview)
                                                model.debug
                                      }
                                    , Cmd.none
                                    )

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( { modelWithUiEvent | ui = nextUi }
            , Cmd.batch
                [ uiEventCmd
                , uiCmd |> Cmd.map UIMsg
                ]
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
            -- Message.asCmd ToggleSimulationActive
            Cmd.none

        Liikennematto.TriggerInGameEffects ->
            Browser.Dom.getViewportOf containerId
                |> Task.andThen
                    (\domViewport ->
                        Browser.Dom.setViewportOf
                            containerId
                            ((domViewport.scene.width - domViewport.viewport.width) / 2)
                            ((domViewport.scene.height - domViewport.viewport.height) / 2)
                    )
                |> Task.attempt (\_ -> NoOp)



--
-- UI events
--


onUiButtonPressed : UI.Model.ButtonKind -> Liikennematto -> ( Liikennematto, Cmd Message )
onUiButtonPressed buttonId model =
    case buttonId of
        UI.Model.PauseSimulation ->
            ( { model | simulationActive = False }, Cmd.none )

        UI.Model.ResumeSimulation ->
            ( { model | simulationActive = True }, Cmd.none )

        UI.Model.NewGame ->
            let
                ( modelWithTransition, transitionActions ) =
                    Liikennematto.triggerLoading model
            in
            ( Liikennematto.fromNewGame modelWithTransition
            , Cmd.batch
                [ gameActionsToCmd transitionActions
                , Savegame.clearSavegameUrl ()
                ]
            )

        UI.Model.SpawnCar ->
            Simulation.spawnCar model

        UI.Model.ToggleCarDebug ->
            ( { model | debug = Model.Debug.toggleLayer Model.Debug.CarDebug model.debug }
            , Cmd.none
            )

        UI.Model.ToggleLotDebug ->
            ( { model | debug = Model.Debug.toggleLayer Model.Debug.LotDebug model.debug }
            , Cmd.none
            )

        UI.Model.ToggleRoadNetworkDebug ->
            ( { model | debug = Model.Debug.toggleLayer Model.Debug.RoadNetworkDebug model.debug }
            , Cmd.none
            )


onViewportChanged : Float -> Float -> Bool -> Liikennematto -> ( Liikennematto, Cmd Message )
onViewportChanged deltaX deltaY shouldSnap model =
    let
        nextViewport =
            model.viewport
                |> Viewport.applyPanDelta deltaX deltaY
                |> Viewport.clamp model.renderCache.tilemapWidthPixels model.renderCache.tilemapHeightPixels
                |> (if shouldSnap then
                        Viewport.snapToEven

                    else
                        identity
                   )
    in
    ( { model | viewport = nextViewport }
    , Cmd.none
    )


updateViewportSize : Int -> Int -> Liikennematto -> Viewport.Viewport
updateViewportSize width height model =
    let
        currentViewport =
            model.viewport

        nextWidth =
            toFloat width

        nextHeight =
            toFloat height

        centerX =
            currentViewport.x + currentViewport.width / 2

        centerY =
            currentViewport.y + currentViewport.height / 2

        nextX =
            centerX - nextWidth / 2

        nextY =
            centerY - nextHeight / 2
    in
    { currentViewport
        | width = nextWidth
        , height = nextHeight
        , x = nextX
        , y = nextY
    }
        |> Viewport.clamp model.renderCache.tilemapWidthPixels model.renderCache.tilemapHeightPixels


onZoomLevelChanged : UI.Model.ZoomLevel -> Liikennematto -> ( Liikennematto, Cmd Message )
onZoomLevelChanged nextZoomLevel model =
    let
        nextRenderCache =
            model.renderCache
                |> setPixelsToMetersRatio nextZoomLevel
                |> setTilemapCache model.world.tilemap Nothing

        tilemapSizeChangeX =
            nextRenderCache.tilemapWidthPixels - model.renderCache.tilemapWidthPixels

        tilemapSizeChangeY =
            nextRenderCache.tilemapHeightPixels - model.renderCache.tilemapHeightPixels

        nextViewport =
            { x = model.viewport.x + (tilemapSizeChangeX / 2)
            , y = model.viewport.y + (tilemapSizeChangeY / 2)
            , width = model.viewport.width
            , height = model.viewport.height
            }
                |> Viewport.clamp nextRenderCache.tilemapWidthPixels nextRenderCache.tilemapHeightPixels

        ui =
            model.ui

        nextEditor =
            Editor.onViewportChanged nextRenderCache nextViewport ui.editor

        nextUi =
            { ui | editor = nextEditor }
    in
    ( { model
        | renderCache = nextRenderCache
        , viewport = nextViewport
        , ui = nextUi
      }
    , Cmd.none
    )


setPixelsToMetersRatio : UI.Model.ZoomLevel -> RenderCache -> RenderCache
setPixelsToMetersRatio zoomLevel cache =
    let
        nextPixelsPerMeter =
            case zoomLevel of
                UI.Model.VeryFar ->
                    4

                UI.Model.Far ->
                    6

                UI.Model.Near ->
                    8

        nextRatio =
            Pixels.pixels nextPixelsPerMeter |> Quantity.per (Length.meters 1)
    in
    { cache
        | pixelsToMetersRatio = nextRatio
        , tilemapWidthPixels = toPixelsValue nextRatio cache.tilemapWidth
        , tilemapHeightPixels = toPixelsValue nextRatio cache.tilemapHeight
    }



--
-- View
--


view : Liikennematto -> Browser.Document Message
view model =
    { title = "Liikennematto"
    , body =
        [ case Liikennematto.currentState model of
            Liikennematto.InGame ->
                UI.view
                    model
                    (render model)
                    (renderDebug model)
                    |> Html.map UIMsg

            Liikennematto.Error ->
                UI.ErrorScreen.view model.errorMessage

            -- Show the splash screen for both the game init and loading states
            _ ->
                UI.SplashScreen.view model.screen
        ]
    }


render : Liikennematto -> Element msg
render model =
    Render.view model.world model.renderCache (Just model.viewport)
        |> Element.html


renderDebug : Liikennematto -> Element msg
renderDebug model =
    Render.Debug.view model.world model.renderCache model.debug (Just model.viewport)
        |> Element.html
