module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Element exposing (Element)
import FSM
import Message exposing (Message(..))
import Model.Flags as Flags exposing (FlagsJson)
import Model.Liikennematto as Liikennematto
    exposing
        ( Liikennematto
        , SimulationState(..)
        )
import Model.Screen as Screen
import Render
import Render.Debug
import Simulation.Simulation as Simulation
import Subscriptions exposing (subscriptions)
import Task
import UI
import UI.ErrorScreen
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


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    updateBase msg model
        |> withUpdate Simulation.update msg
        |> withUpdate UI.update msg


updateBase : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
updateBase msg model =
    case msg of
        VisibilityChanged newVisibility ->
            ( { model
                | simulation =
                    case newVisibility of
                        Events.Visible ->
                            model.simulation

                        Events.Hidden ->
                            Paused
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

        AnimationFrameReceived delta ->
            let
                ( nextGameFSM, gameActions ) =
                    FSM.update delta model.initSteps model.game
            in
            ( { model | game = nextGameFSM }
            , gameActionsToCmd gameActions
            )

        NewGame ->
            let
                previousWorld =
                    Just (Simulation.worldAfterTilemapChange model.world)

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
            Message.asCmd (SetSimulation Liikennematto.Running)

        Liikennematto.TriggerInGameEffects ->
            Message.asCmd Message.InGame


view : Liikennematto -> Browser.Document Message
view model =
    { title = "Liikennematto"
    , body =
        [ case Liikennematto.currentState model of
            Liikennematto.InGame ->
                UI.layout
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


render : Liikennematto -> Element msg
render model =
    Render.view model.world model.renderCache model.dynamicTiles
        |> Element.html


renderDebug : Liikennematto -> Element msg
renderDebug model =
    let
        debugLayers =
            { showRoadNetwork = model.showRoadNetwork
            , showCarDebugVisuals = model.showCarDebugVisuals
            }
    in
    if debugLayers.showRoadNetwork || debugLayers.showCarDebugVisuals then
        Render.Debug.view model.world model.renderCache debugLayers
            |> Element.html

    else
        Element.none
