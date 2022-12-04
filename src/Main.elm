module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Element exposing (Element)
import FSM
import Message exposing (Message(..))
import Model.Liikennematto as Liikennematto
    exposing
        ( GameState(..)
        , Liikennematto
        , SimulationState(..)
        )
import Model.Screen as Screen
import Random
import Render
import Render.Debug
import Simulation.Simulation as Simulation
import Subscriptions exposing (subscriptions)
import Task
import Time
import UI


main : Program () Liikennematto Message
main =
    let
        initialModel =
            Liikennematto.initial

        initCmds =
            Cmd.batch
                [ -- simulate a screen resize
                  Task.perform WindowResized getViewport
                , Task.perform ResetSeed Time.now
                ]
    in
    Browser.document
        { init = \() -> ( initialModel, initCmds )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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
                nextScreen =
                    Screen.fromDimensions (round domViewport.viewport.width) (round domViewport.viewport.height)
            in
            ( { model | screen = nextScreen }
            , Cmd.none
            )

        ResetSeed posix ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis posix) }
            , Cmd.none
            )

        AnimationFrameReceived delta ->
            let
                ( nextGameFSM, gameActions ) =
                    FSM.update delta () model.game
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


view : Liikennematto -> Browser.Document Message
view model =
    { title = "Liikennematto"
    , body =
        [ case Liikennematto.currentState model of
            InGame ->
                UI.layout
                    model
                    (render model)
                    (renderDebug model)

            Error ->
                UI.errorScreen

            -- Show the splash screen for both the game init and loading states
            _ ->
                UI.splashScreen
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
