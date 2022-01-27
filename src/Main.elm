module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Message exposing (Message(..))
import Model.Liikennematto as Liikennematto exposing (Liikennematto, SimulationState(..))
import Random
import Render
import Simulation.Simulation as Simulation
import Subscriptions exposing (subscriptions)
import Task
import UI.Core exposing (borderRadius, colors)
import UI.Editor
import UI.UI as UI


main : Program () Liikennematto Message
main =
    let
        initialModel =
            Liikennematto.new
    in
    Browser.document
        { init = \() -> ( initialModel, initCmd initialModel.seed )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initCmd : Random.Seed -> Cmd Message
initCmd seed =
    Cmd.batch
        [ -- simulate a screen resize
          Task.perform (\{ viewport } -> ResizeWindow (round viewport.width) (round viewport.height)) getViewport
        , Simulation.initCmd seed
        ]


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

        ResizeWindow width height ->
            ( { model
                | screen =
                    { width = width
                    , height = height
                    , orientation =
                        if width < height then
                            Element.Portrait

                        else
                            Element.Landscape
                    }
              }
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

        renderedSize =
            Render.tilemapSizePixels |> floor

        ( viewportWidth, viewportHeight ) =
            ( min model.screen.width renderedSize, min model.screen.height renderedSize )

        overflowStrategy =
            if renderedSize > model.screen.width || renderedSize > model.screen.height then
                Element.scrollbars

            else
                Element.clip
    in
    Render.view model.world model.renderCache debugLayers
        |> Element.html
        -- render + overlay
        |> Element.el
            [ Element.width (Element.px renderedSize)
            , Element.height (Element.px renderedSize)
            , Element.inFront (UI.Editor.overlay model.world model.tool)
            , Background.color colors.terrain
            ]
        -- overflow wrapper
        |> Element.el
            [ Element.width (Element.px viewportWidth)
            , Element.height (Element.px viewportHeight)
            , Element.centerX
            , Element.centerY
            , Border.rounded borderRadius.heavy
            , overflowStrategy
            ]
