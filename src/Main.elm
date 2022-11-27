module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Element exposing (Element)
import Element.Border as Border
import Message exposing (Message(..))
import Model.Liikennematto as Liikennematto exposing (Liikennematto, SimulationState(..))
import Model.Screen as Screen
import Random
import Render
import Render.Debug
import Simulation.Simulation as Simulation
import Subscriptions exposing (subscriptions)
import Task
import Time
import UI.Core exposing (borderRadiusButton, borderSize, colors, uiDimensions)
import UI.Editor
import UI.UI as UI


main : Program () Liikennematto Message
main =
    let
        ( initialModel, editorCmd ) =
            UI.Editor.init Liikennematto.new

        initCmds =
            Cmd.batch
                [ -- simulate a screen resize
                  Task.perform WindowResized getViewport
                , Task.perform ResetSeed Time.now
                , Simulation.initCmd initialModel.seed
                , editorCmd
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
    , body = [ UI.layout model render ]
    }


render : Liikennematto -> Element Message
render model =
    let
        debugLayers =
            { showRoadNetwork = model.showRoadNetwork
            , showCarDebugVisuals = model.showCarDebugVisuals
            }

        renderWidth =
            floor model.renderCache.tilemapWidthPixels + (borderSize * 2)

        renderHeight =
            floor model.renderCache.tilemapHeightPixels + (borderSize * 2)

        wrapperWidth =
            renderWidth + (borderSize * 2) + uiDimensions.renderSafeAreaX

        wrapperHeight =
            renderHeight + (borderSize * 2) + uiDimensions.renderSafeAreaY

        horizontalAlignment =
            if wrapperWidth < model.screen.width then
                Element.centerX

            else
                Element.alignLeft

        ( verticalAlignment, renderTopOffset ) =
            if wrapperHeight < model.screen.height then
                ( Element.centerY, 0 )

            else
                ( Element.alignTop, borderRadiusButton )

        renderDebug =
            if debugLayers.showRoadNetwork || debugLayers.showCarDebugVisuals then
                Render.Debug.view model.world model.renderCache debugLayers
                    |> Element.html

            else
                Element.none
    in
    Render.view model.world model.renderCache model.dynamicTiles
        |> Element.html
        -- render + overlay
        |> Element.el
            [ Element.width (Element.px renderWidth)
            , Element.height (Element.px renderHeight)
            , Element.moveDown renderTopOffset
            , Element.centerX
            , Element.clip
            , verticalAlignment
            , Border.solid
            , Border.rounded borderRadiusButton
            , Border.width borderSize
            , Border.color colors.renderBorder
            , Element.inFront renderDebug
            , Element.inFront
                (UI.Editor.overlay
                    model.renderCache
                    model.world
                    model.editor
                )
            ]
        |> Element.el
            [ Element.width (Element.px wrapperWidth)
            , Element.height (Element.px wrapperHeight)
            , horizontalAlignment
            , verticalAlignment
            ]
