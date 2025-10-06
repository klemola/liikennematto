module UI.Button exposing
    ( ButtonConfig
    , iconButton
    , iconWithTextButton
    , roundIconButton
    , textButton
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as HtmlAttribute
import Svg exposing (Svg)
import UI.Core
    exposing
        ( uiColorBorder
        , uiColorText
        , whitespaceRegular
        )


defaultBorderRadiusPx : Int
defaultBorderRadiusPx =
    8


borderSizePx : Int
borderSizePx =
    2


buttonHeightPx : Int
buttonHeightPx =
    42


type alias ButtonConfig msg =
    { onPress : msg
    , selected : Bool
    , disabled : Bool
    }


bgColor =
    Element.rgb255 232 236 242


bgColorActive =
    Element.rgb255 253 253 247


borderColorActive =
    Element.rgb255 69 133 196


buttonSize baseSize =
    Element.px (baseSize - (2 * borderSizePx))


iconButton : ButtonConfig msg -> Int -> Svg msg -> Element msg
iconButton =
    createIconButton defaultBorderRadiusPx


roundIconButton : ButtonConfig msg -> Int -> Svg msg -> Element msg
roundIconButton config buttonSizePx icon =
    createIconButton (buttonSizePx // 2) config buttonSizePx icon


createIconButton : Int -> ButtonConfig msg -> Int -> Svg msg -> Element msg
createIconButton borderRadiusPx { onPress, selected, disabled } buttonSizePx icon =
    let
        width =
            buttonSize buttonSizePx

        height =
            buttonSize buttonSizePx

        alpha =
            if disabled then
                0.65

            else
                1
    in
    Input.button
        [ Element.width width
        , Element.height height
        , Element.padding 0
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver
            [ Background.color bgColorActive ]
        , Element.mouseDown
            [ Background.color bgColorActive ]
        , Background.color
            (if selected then
                bgColorActive

             else
                bgColor
            )
        , Border.width borderSizePx
        , Border.rounded borderRadiusPx
        , Border.solid
        , Border.color
            (if selected then
                borderColorActive

             else
                uiColorBorder
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = Element.html icon
        }


iconWithTextButton : ButtonConfig msg -> String -> Svg msg -> Element msg
iconWithTextButton { onPress, selected, disabled } textContent icon =
    let
        height =
            Element.px buttonHeightPx

        alpha =
            if disabled then
                0.5

            else
                1
    in
    Input.button
        [ Background.color bgColor
        , Element.width Element.fill
        , Element.height Element.shrink
        , Element.alpha alpha
        , Element.mouseOver
            [ Background.color bgColorActive ]
        , Element.mouseDown
            [ Background.color bgColorActive ]
        , Border.width borderSizePx
        , Border.rounded defaultBorderRadiusPx
        , Border.solid
        , Border.color
            (if selected then
                borderColorActive

             else
                uiColorBorder
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label =
            Element.row
                [ Element.spacing whitespaceRegular
                , Element.width Element.fill
                , Element.height height
                , Element.clip
                , Element.htmlAttribute (HtmlAttribute.class "lm-ui-button")
                ]
                [ Element.html
                    (Html.node "style"
                        []
                        [ Html.text ".lm-ui-button svg {width:42px;height: 42px;border-radius:8px 0 0 8px}" ]
                    )
                , Element.el
                    [ Element.width height
                    , Element.height Element.fill
                    , Element.clip
                    , Border.solid
                    , Border.color uiColorBorder
                    , Border.widthEach
                        { right = 1
                        , top = 0
                        , bottom = 0
                        , left = 0
                        }
                    ]
                    (Element.html icon)
                , Element.el
                    [ Element.width Element.fill
                    , Element.centerY
                    , Font.bold
                    , Font.size 14
                    , Font.color uiColorText
                    , Font.letterSpacing 0.5
                    ]
                    (Element.text textContent)
                ]
        }


textButton : ButtonConfig msg -> String -> Element msg
textButton { onPress, selected, disabled } textContent =
    let
        width =
            Element.px 100

        alpha =
            if disabled then
                0.5

            else
                1
    in
    Input.button
        [ Background.color bgColor
        , Element.width width
        , Element.height Element.fill
        , Element.paddingXY 8 4
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver
            [ Background.color bgColorActive ]
        , Element.mouseDown
            [ Background.color bgColorActive ]
        , Font.size 14
        , Font.color uiColorText
        , Font.bold
        , Font.letterSpacing 0.5
        , Border.width borderSizePx
        , Border.rounded defaultBorderRadiusPx
        , Border.solid
        , Border.color
            (if selected then
                borderColorActive

             else
                uiColorBorder
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = Element.text textContent
        }
