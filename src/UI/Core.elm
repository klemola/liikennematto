module UI.Core exposing (..)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias UiDimensions =
    { toolbar : Int
    , controlButtonS : Int
    , controlButtonL : Int
    , panel : Int
    , overlay : Int
    , text : Int
    , smallControlsBreakpoint : Int
    }


uiDimensions : UiDimensions
uiDimensions =
    { toolbar = 80
    , controlButtonS = 42
    , controlButtonL = 64
    , panel = 256
    , overlay = 256
    , text = 14
    , smallControlsBreakpoint = 720
    }


colors =
    { mainBackground = Element.rgb255 88 135 140
    , menuBackground = Element.rgb255 159 192 198
    , buttonBackground = Element.rgb255 228 228 235
    , listItemBackground = Element.rgb255 109 151 156
    , text = Element.rgb255 52 65 67
    , textInverse = Element.rgb255 222 222 222
    , link = Element.rgb255 10 132 199
    , selected = Element.rgb255 242 212 13
    , danger = Element.rgb255 235 119 52
    , notAllowed = Element.rgb255 245 66 84
    , target = Element.rgb255 222 222 222
    , terrain = Element.rgb255 33 191 154
    , transparent = Element.rgba255 0 0 0 0
    , lightBorder = Element.rgb255 220 220 226
    , heavyBorder = Element.rgb255 53 93 97
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


type ControlButtonSize
    = CBSmall
    | CBLarge


controlButton :
    { label : Element msg
    , onPress : msg
    , selected : Bool
    , disabled : Bool
    , size : ControlButtonSize
    }
    -> Element msg
controlButton { label, onPress, selected, disabled, size } =
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

        alpha =
            if disabled then
                0.5

            else
                1
    in
    Input.button
        [ Background.color colors.buttonBackground
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


link : String -> String -> Element msg
link url label =
    Element.newTabLink
        [ Font.color colors.link
        ]
        { url = url
        , label = Element.text label
        }
