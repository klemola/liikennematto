module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Config exposing (boardSizeScaled)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Message exposing (Message(..))
import Model.Liikennematto as Liikennematto exposing (Liikennematto)
import Pixels
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
        { init = \() -> ( initialModel, initMessages initialModel.seed )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


initMessages : Random.Seed -> Cmd Message
initMessages seed =
    Cmd.batch
        [ -- simulate a screen resize
          Task.perform (\{ viewport } -> ResizeWindow (round viewport.width) (round viewport.height)) getViewport
        , Simulation.generateEnvironmentAfterDelay seed
        ]


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
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
            let
                ( modelWithSimulationChanges, simulationCmd ) =
                    Simulation.update msg model

                ( modelWithUIChanges, uiCmd ) =
                    UI.update msg modelWithSimulationChanges
            in
            ( modelWithUIChanges, Cmd.batch [ simulationCmd, uiCmd ] )


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
            boardSizeScaled |> Pixels.inPixels

        ( viewportWidth, viewportHeight ) =
            ( min model.screen.width renderedSize, min model.screen.height renderedSize )

        overflowStrategy =
            if renderedSize > model.screen.width || renderedSize > model.screen.height then
                Element.scrollbars

            else
                Element.clip
    in
    Render.view model.world debugLayers
        |> Element.html
        -- render + overlay
        |> Element.el
            [ Element.width (Element.px renderedSize)
            , Element.height (Element.px renderedSize)
            , Element.inFront (UI.Editor.overlay model)
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
