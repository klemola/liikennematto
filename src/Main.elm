module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Element exposing (Element)
import Message exposing (Message(..))
import Model.Liikennematto as Liikennematto
    exposing
        ( Liikennematto
        , SimulationState(..)
        )
import Model.Screen as Screen
import Process
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
                , Process.sleep 1000
                    |> Task.perform (always GameSetupComplete)
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

        NewGame ->
            let
                previousWorld =
                    Just (Simulation.worldAfterTilemapChange model.world)
            in
            ( Liikennematto.fromNewGame previousWorld model
            , Cmd.none
            )

        RestoreGame ->
            ( Liikennematto.fromPreviousGame model
            , Cmd.none
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


view : Liikennematto -> Browser.Document Message
view model =
    { title = "Liikennematto"
    , body =
        [ UI.layout
            model
            (render model)
            (renderDebug model)
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
