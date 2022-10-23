module UI.Core exposing
    ( ControlButtonProperties
    , UiDimensions
    , borderRadius
    , borderSize
    , colors
    , containerId
    , controlButton
    , icon
    , preventDefaultLongTap
    , scrollbarAwareOffsetF
    , smallControlButton
    , uiDimensions
    , whitespace
    )

import Data.Colors as Colors exposing (uiCompat)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Events
import Json.Decode


containerId : String
containerId =
    "liikennematto"


type alias UiDimensions =
    { controlButton : Int
    , panel : Int
    , panelVerticalMargin : Int
    , renderSafeAreaX : Int
    , renderSafeAreaY : Int
    , overlay : Int
    , text : Int
    , zoomControlWidth : Int
    , zoomTrackHeight : Int
    , zoomTrackWidth : Int
    }


baseSpacing : Int
baseSpacing =
    20


scrollbarAwareOffset : Int
scrollbarAwareOffset =
    baseSpacing


scrollbarAwareOffsetF : Float
scrollbarAwareOffsetF =
    toFloat scrollbarAwareOffset


uiDimensions : UiDimensions
uiDimensions =
    { controlButton = baseSpacing * 3
    , panel = 256
    , panelVerticalMargin = baseSpacing * 2
    , renderSafeAreaX = baseSpacing * 4 + scrollbarAwareOffset
    , renderSafeAreaY = baseSpacing * 4 + scrollbarAwareOffset
    , overlay = 256
    , text = 14
    , zoomControlWidth = baseSpacing * 2
    , zoomTrackHeight = baseSpacing * 8
    , zoomTrackWidth = 14
    }


colors =
    { mainBackground = uiCompat Colors.lightGreen
    , menuBackground = uiCompat (Colors.withAlpha 0.6 Colors.darkGreen)
    , inputBackground = uiCompat Colors.gray5
    , listItemBackground = uiCompat Colors.gray5
    , text = uiCompat Colors.gray2
    , textInverse = uiCompat Colors.gray6
    , link = uiCompat Colors.darkBlue
    , selected = uiCompat Colors.darkBlue
    , danger = uiCompat Colors.yellow
    , notAllowed = uiCompat Colors.red
    , target = uiCompat Colors.gray6
    , transparent = Element.rgba255 0 0 0 0
    , renderBorder = uiCompat Colors.darkGreen
    , lightBorder = uiCompat Colors.gray4
    , heavyBorder = uiCompat Colors.gray1
    }


whitespace =
    { regular = 10
    , spacious = 20
    , tight = 5
    }


borderSize =
    { heavy = 10
    , light = 3
    }


borderRadius =
    { heavy = 5
    , light = 3
    }


type alias ControlButtonProperties msg =
    { label : Element msg
    , onPress : msg
    , selected : Bool
    , disabled : Bool
    }


type ControlButtonSize
    = Large
    | Small


controlButton : ControlButtonProperties msg -> Element msg
controlButton =
    buildControlButton Large


smallControlButton : ControlButtonProperties msg -> Element msg
smallControlButton =
    buildControlButton Small


buildControlButton : ControlButtonSize -> ControlButtonProperties msg -> Element msg
buildControlButton size { label, onPress, selected, disabled } =
    let
        baseSize =
            case size of
                Small ->
                    uiDimensions.controlButton // 2

                Large ->
                    uiDimensions.controlButton

        buttonSize =
            Element.px (baseSize - (2 * borderSize.light))

        fontSize =
            baseSize // 2

        alpha =
            if disabled then
                0.5

            else
                1
    in
    Input.button
        [ Background.color colors.inputBackground
        , Font.size fontSize
        , Font.center
        , Element.width buttonSize
        , Element.height buttonSize
        , Element.alpha alpha
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
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = label
        }


icon : String -> Element msg
icon filename =
    Element.image [ Element.width Element.fill ] { description = "", src = "assets/" ++ filename }


preventDefaultLongTap : msg -> Element.Attribute msg
preventDefaultLongTap msg =
    Element.htmlAttribute
        (Html.Events.custom
            "touchstart"
            (Json.Decode.succeed
                { message = msg
                , stopPropagation = True
                , preventDefault = True
                }
            )
        )
