module Main exposing (main)

import Audio
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Duration exposing (Duration)
import Element exposing (Element)
import Html
import Json.Decode as Decode
import Lib.FSM as FSM
import Message exposing (Message(..))
import Model.Debug
import Model.Flags as Flags exposing (FlagsJson)
import Model.Liikennematto as Liikennematto exposing (Liikennematto)
import Model.RenderCache
import Model.Screen as Screen
import Render
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

                zoomedViewport =
                    viewportForZoom model.ui.zoomLevel
                        { screenWidth = toFloat (round domViewport.viewport.width)
                        , screenHeight = toFloat (round domViewport.viewport.height)
                        }

                nextRenderCache =
                    model.renderCache
                        |> Model.RenderCache.updatePannableBounds zoomedViewport.width zoomedViewport.height

                unclampedViewport =
                    if initSteps.viewportSizeSet then
                        -- Subsequent resizes: maintain center
                        updateViewportSize
                            (round domViewport.viewport.width)
                            (round domViewport.viewport.height)
                            model

                    else
                        -- First load: center tilemap in viewport
                        { x = (model.renderCache.tilemapWidthPixels - zoomedViewport.width) / 2
                        , y = (model.renderCache.tilemapHeightPixels - zoomedViewport.height) / 2
                        , width = zoomedViewport.width
                        , height = zoomedViewport.height
                        }

                centeredViewport =
                    Viewport.clampWithBounds nextRenderCache.pannableBounds unclampedViewport

                ui =
                    model.ui

                nextEditor =
                    Editor.onViewportChanged nextRenderCache centeredViewport ui.editor

                nextUi =
                    { ui | editor = nextEditor }
            in
            ( { model
                | screen = nextScreen
                , initSteps = nextInitSteps
                , viewport = centeredViewport
                , renderCache = nextRenderCache
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
                        , renderCache = Model.RenderCache.clear model.viewport model.renderCache worldWithResidents
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
                    UI.update model.world model.viewport model.screen uiMsg model.ui

                modelWithNextUi =
                    { model | ui = nextUi }

                ( modelWithUiEvent, uiEventCmd ) =
                    case uiEvent of
                        Just event ->
                            case event of
                                UI.GameInputReceived inputEvent ->
                                    case inputEvent.kind of
                                        UI.Core.Primary ->
                                            Tilemap.onPrimaryInput inputEvent.cell modelWithNextUi

                                        UI.Core.Secondary ->
                                            Tilemap.onSecondaryInput inputEvent.cell modelWithNextUi

                                UI.ButtonPressed buttonId ->
                                    onUiButtonPressed buttonId modelWithNextUi

                                UI.ZoomLevelChanged zoomLevel ->
                                    onZoomLevelChanged zoomLevel modelWithNextUi

                                UI.ViewportChanged deltaX deltaY shouldSnap ->
                                    onViewportChanged deltaX deltaY shouldSnap modelWithNextUi

                                UI.DevViewSelected devView ->
                                    ( { modelWithNextUi
                                        | debug =
                                            Model.Debug.setLayer Model.Debug.WFCDebug
                                                (devView == UI.Model.WFCOverview)
                                                modelWithNextUi.debug
                                      }
                                    , Cmd.none
                                    )

                        Nothing ->
                            ( modelWithNextUi, Cmd.none )
            in
            ( modelWithUiEvent
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
            ( { model | simulationActive = False }
            , Cmd.none
            )

        UI.Model.ResumeSimulation ->
            ( { model | simulationActive = True }
            , Cmd.none
            )

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
        scaleX =
            model.viewport.width / toFloat model.screen.width

        scaleY =
            model.viewport.height / toFloat model.screen.height

        scaledDeltaX =
            deltaX * scaleX

        scaledDeltaY =
            deltaY * scaleY

        nextViewport =
            model.viewport
                |> Viewport.applyPanDelta scaledDeltaX scaledDeltaY
                |> Viewport.clampWithBounds model.renderCache.pannableBounds
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
updateViewportSize screenWidth screenHeight model =
    let
        currentViewport =
            model.viewport

        nextViewport =
            viewportForZoom model.ui.zoomLevel
                { screenWidth = toFloat screenWidth
                , screenHeight = toFloat screenHeight
                }

        centerX =
            currentViewport.x + currentViewport.width / 2

        centerY =
            currentViewport.y + currentViewport.height / 2
    in
    { currentViewport
        | width = nextViewport.width
        , height = nextViewport.height
        , x = centerX - nextViewport.width / 2
        , y = centerY - nextViewport.height / 2
    }


onZoomLevelChanged : UI.Model.ZoomLevel -> Liikennematto -> ( Liikennematto, Cmd Message )
onZoomLevelChanged nextZoomLevel model =
    let
        viewportWithZoom =
            viewportForZoom nextZoomLevel
                { screenWidth = toFloat model.screen.width
                , screenHeight = toFloat model.screen.height
                }

        centerX =
            model.viewport.x + model.viewport.width / 2

        centerY =
            model.viewport.y + model.viewport.height / 2

        nextRenderCache =
            model.renderCache
                |> Model.RenderCache.updatePannableBounds viewportWithZoom.width viewportWithZoom.height

        centeredViewport =
            { x = centerX - viewportWithZoom.width / 2
            , y = centerY - viewportWithZoom.height / 2
            , width = viewportWithZoom.width
            , height = viewportWithZoom.height
            }
                |> Viewport.clampWithBounds nextRenderCache.pannableBounds

        ui =
            model.ui

        nextEditor =
            Editor.onViewportChanged nextRenderCache centeredViewport ui.editor

        nextUi =
            { ui | editor = nextEditor }
    in
    ( { model
        | renderCache = nextRenderCache
        , viewport = centeredViewport
        , ui = nextUi
      }
    , Cmd.none
    )


viewportForZoom : UI.Model.ZoomLevel -> { screenWidth : Float, screenHeight : Float } -> { width : Float, height : Float }
viewportForZoom zoomLevel { screenWidth, screenHeight } =
    let
        cellSizePixels =
            256

        -- Target visible cells amount in the bigger dimensions of the screen
        targetCellsHeight =
            case zoomLevel of
                UI.Model.Near ->
                    5

                UI.Model.Far ->
                    10

                UI.Model.VeryFar ->
                    -- Tilemap is 12 cells; show it at 90% of viewport
                    -- TODO: don't use hardcoded value
                    12 / 0.9

        viewportHeight =
            targetCellsHeight * cellSizePixels

        aspectRatio =
            screenWidth / screenHeight

        viewportWidth =
            viewportHeight * aspectRatio
    in
    { width = viewportWidth, height = viewportHeight }



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
    Render.view model.world model.renderCache model.screen (Just model.viewport)
        |> Element.html


renderDebug : Liikennematto -> Element msg
renderDebug model =
    Render.Debug.view model.world model.renderCache model.debug model.screen (Just model.viewport)
        |> Element.html
