module UI exposing (ControlButtonSize(..), controlButton, icon, projectInfo, simulationControl)

import Config exposing (borderRadius, borderSize, colors, uiDimensions, whitespace)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Simulation exposing (Msg(..), SimulationState(..))


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
    controlButton { label = Element.text label, onPress = msg, selected = selected, size = controlButtonSize }


type ControlButtonSize
    = CBSmall
    | CBLarge


controlButton :
    { label : Element msg
    , onPress : msg
    , selected : Bool
    , size : ControlButtonSize
    }
    -> Element msg
controlButton { label, onPress, selected, size } =
    let
        ( buttonSize, fontSize ) =
            case size of
                CBSmall ->
                    ( Element.px (uiDimensions.controlButtonS - (2 * borderSize.light))
                    , uiDimensions.controlButtonS // 2
                    )

                CBLarge ->
                    ( Element.px (uiDimensions.controlButtonL - (2 * borderSize.light))
                    , uiDimensions.controlButtonL // 2
                    )
    in
    Input.button
        [ Background.color colors.buttonBackground
        , Font.size fontSize
        , Font.center
        , Element.width buttonSize
        , Element.height buttonSize
        , Border.width borderSize.light
        , Border.rounded borderRadius.light
        , Border.solid
        , Border.color
            (if selected then
                colors.selected

             else
                colors.lightBorder
            )
        ]
        { onPress = Just onPress
        , label = label
        }


icon : String -> Element msg
icon filename =
    Element.image [ Element.width Element.fill ] { description = "", src = "assets/" ++ filename }


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


link : String -> String -> Element msg
link url label =
    Element.newTabLink
        [ Font.color colors.link
        ]
        { url = url
        , label = Element.text label
        }
