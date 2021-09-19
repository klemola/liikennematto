module UI.UI exposing (carSpawnControl, projectInfo, simulationControl)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Simulation.Simulation as Simulation exposing (Msg(..), SimulationState(..))
import UI.Core exposing (ControlButtonSize, borderRadius, colors, controlButton, link, uiDimensions, whitespace)


simulationControl : Simulation.Model -> ControlButtonSize -> Element Simulation.Msg
simulationControl { simulation } controlButtonSize =
    let
        ( label, selected, msg ) =
            case simulation of
                Running ->
                    ( "⏸️", False, SetSimulation Paused )

                Paused ->
                    ( "▶️", True, SetSimulation Running )
    in
    controlButton
        { label = Element.text label
        , onPress = msg
        , selected = selected
        , disabled = False
        , size = controlButtonSize
        }


carSpawnControl : Simulation.Model -> ControlButtonSize -> Element Simulation.Msg
carSpawnControl { carSpawnQueue } controlButtonSize =
    let
        disabled =
            carSpawnQueue >= Simulation.maxCarSpawnQueueSize
    in
    controlButton
        { label = Element.text "🚗"
        , onPress = Simulation.SpawnTestCar
        , selected = False
        , disabled = disabled
        , size = controlButtonSize
        }


projectInfo : Element msg
projectInfo =
    Element.row
        [ Element.width Element.shrink
        , Element.padding whitespace.regular
        , Element.spacing whitespace.regular
        , Element.alignRight
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.typeface "sans-serif"
            ]
        , Font.size uiDimensions.text
        , Background.color colors.textInverse
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = borderRadius.heavy
            }
        ]
        [ Element.el [ Font.bold ] (Element.text "Liikennematto")
        , Element.row
            [ Element.spacing whitespace.tight
            , Element.alignRight
            ]
            [ link "https://github.com/klemola/liikennematto" "GitHub"
            , Element.text "｜"
            , link "https://matiasklemola.com/liikennematto-dev-blog-one" "Blog"
            , Element.text "｜"
            , link "https://twitter.com/MatiasKlemola" "Twitter"
            ]
        ]
