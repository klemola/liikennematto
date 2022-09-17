module UI.Core exposing
    ( ControlButtonSize(..)
    , UiDimensions
    , borderRadius
    , borderSize
    , colors
    , controlButton
    , icon
    , link
    , uiDimensions
    , whitespace
    )

import Color
import Data.Colors as Colors exposing (uiCompat)
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
    , renderSafeAreaX : Int
    , renderSafeAreaY : Int
    , renderOffsetY : Float
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
    , renderSafeAreaX = 40
    , renderSafeAreaY = 120
    , renderOffsetY = 40
    , overlay = 256
    , text = 14
    , smallControlsBreakpoint = 720
    }


colors =
    { mainBackground = uiCompat Colors.lightGreen
    , menuBackground = uiCompat (Colors.withAlpha 0.6 Colors.darkGreen)
    , buttonBackground = uiCompat Colors.gray4
    , listItemBackground = uiCompat Colors.gray2
    , text = uiCompat Colors.gray2
    , textInverse = uiCompat Colors.gray6
    , link = uiCompat Colors.darkBlue
    , selected = uiCompat Colors.darkBlue
    , danger = uiCompat Colors.yellow
    , notAllowed = uiCompat Colors.red
    , target = uiCompat Colors.gray6
    , transparent = Element.rgba255 0 0 0 0
    , renderBorder = uiCompat Colors.darkGreen
    , lightBorder = uiCompat Colors.gray3
    , heavyBorder = uiCompat Colors.gray3
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
