module UI exposing (controlButton, debug, projectInfo, simulationControl)

import Car exposing (Car)
import Config exposing (borderRadius, borderSize, colors, uiDimensions, whitespace)
import Dict
import Direction exposing (Orientation(..))
import Element
    exposing
        ( Element
        , alignRight
        , clipX
        , column
        , el
        , fill
        , height
        , image
        , newTabLink
        , padding
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphics
import Position
import Simulation exposing (Msg(..), SimulationState(..))
import World exposing (World)


simulationControl : Simulation.Model -> Element Simulation.Msg
simulationControl { simulation } =
    let
        ( label, selected, msg ) =
            case simulation of
                Running ->
                    ( "⏸️", False, SetSimulation Paused )

                Paused ->
                    ( "▶️", True, SetSimulation Running )
    in
    controlButton (text label) msg selected


controlButtonSize : Element.Length
controlButtonSize =
    px (uiDimensions.controlButton - (2 * borderSize.light))


controlButton : Element msg -> msg -> Bool -> Element msg
controlButton label onPress selected =
    Input.button
        [ Background.color colors.buttonBackground
        , Font.size (uiDimensions.controlButton // 2)
        , Font.center
        , width controlButtonSize
        , height controlButtonSize
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


debug : World -> Element msg
debug world =
    el [ Element.paddingXY whitespace.regular 0 ]
        (column
            [ width (px uiDimensions.panel)
            , padding whitespace.regular
            , spacing whitespace.tight
            , Background.color colors.menuBackground
            , Border.rounded borderRadius.heavy
            , Border.solid
            , Border.widthEach
                { top = borderSize.heavy
                , bottom = borderSize.heavy
                , left = borderSize.light
                , right = borderSize.light
                }
            , Border.color colors.heavyBorder
            ]
            [ el
                [ Font.size 16
                , Font.bold
                , Font.color colors.text
                ]
                (text "Debug")
            , column
                [ spacing whitespace.tight
                , width fill
                ]
                (Dict.values world.cars
                    |> List.map (carStateView uiDimensions.text)
                )
            ]
        )


carStateView : Int -> Car -> Element msg
carStateView fontSize car =
    let
        showCarKind =
            image [ width (px fontSize) ]
                { description = ""
                , src = "assets/" ++ Graphics.carAsset car
                }
    in
    row
        [ width fill
        , padding whitespace.tight
        , spacing whitespace.regular
        , clipX
        , Font.color colors.textInverse
        , Font.size 13
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ showCarKind
        , column [ spacing whitespace.tight ]
            [ text (Position.toString car.position)
            , text (Car.statusDescription car.status)
            ]
        ]


projectInfo : Element msg
projectInfo =
    row
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.typeface "sans-serif"
            ]
        , Font.size uiDimensions.text
        , Background.color colors.textInverse
        , Border.rounded borderRadius.light
        , width fill
        , padding whitespace.regular
        , spacing whitespace.regular
        ]
        [ el
            [ Font.size 16
            , Font.bold
            ]
            (text "Liikennematto")
        , row
            [ spacing whitespace.tight
            , alignRight
            ]
            [ link "https://github.com/klemola/liikennematto" "GitHub"
            , text "｜"
            , link "https://matiasklemola.com/liikennematto-dev-blog-one" "Blog"
            , text "｜"
            , link "https://twitter.com/MatiasKlemola" "Twitter"
            ]
        ]


link : String -> String -> Element msg
link url label =
    newTabLink
        [ Font.color colors.link
        ]
        { url = url
        , label = text label
        }
