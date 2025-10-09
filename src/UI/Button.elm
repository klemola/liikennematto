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
    Element.rgb255 222 228 237


bgColorActive =
    Element.rgb255 235 237 239


iconBgColorActive =
    Element.rgb255 157 188 241


borderColorActive =
    Element.rgb255 41 108 225


buttonSize baseSize =
    Element.px (baseSize - (2 * borderSizePx))


iconButton : ButtonConfig msg -> Int -> ( Element.Color, Svg msg ) -> Element msg
iconButton =
    createIconButton defaultBorderRadiusPx


roundIconButton : ButtonConfig msg -> Int -> ( Element.Color, Svg msg ) -> Element msg
roundIconButton config buttonSizePx icon =
    createIconButton (buttonSizePx // 2) config buttonSizePx icon


createIconButton : Int -> ButtonConfig msg -> Int -> ( Element.Color, Svg msg ) -> Element msg
createIconButton borderRadiusPx { onPress, selected, disabled } buttonSizePx icon =
    let
        ( iconBg, iconSvg ) =
            icon

        width =
            buttonSize buttonSizePx

        height =
            buttonSize buttonSizePx

        alpha =
            if disabled then
                0.65

            else
                1

        hoverAttrs =
            [ Background.color iconBgColorActive
            , Border.color borderColorActive
            ]
    in
    Input.button
        [ Element.width width
        , Element.height height
        , Element.padding 0
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver hoverAttrs
        , Element.mouseDown hoverAttrs
        , Background.color
            (if selected then
                iconBgColorActive

             else
                iconBg
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
        , label = Element.html iconSvg
        }


iconWithTextButton : ButtonConfig msg -> String -> ( Element.Color, Svg msg ) -> Element msg
iconWithTextButton { onPress, selected, disabled } textContent icon =
    let
        ( iconBg, iconSvg ) =
            icon

        height =
            Element.px buttonHeightPx

        alpha =
            if disabled then
                0.65

            else
                1

        hoverAttrs =
            [ Background.color bgColorActive
            , Border.color borderColorActive
            ]

        extraCSS =
            """
            .lm-ui-button svg {width:38px;height:38px;}
            .lm-ui-button:hover .lm-ui-button__icon {background:rgb(157,188,241)}
            .lm-ui-button.selected .lm-ui-button__icon {background:rgb(157,188,241)}
            """
    in
    Input.button
        [ Background.color
            (if selected then
                bgColorActive

             else
                bgColor
            )
        , Element.width Element.fill
        , Element.height (Element.px (buttonHeightPx + borderSizePx))
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver hoverAttrs
        , Element.mouseDown hoverAttrs
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
                , Element.htmlAttribute
                    (HtmlAttribute.class
                        (if selected then
                            "lm-ui-button selected"

                         else
                            "lm-ui-button"
                        )
                    )
                ]
                [ Element.html
                    (Html.node "style"
                        []
                        [ Html.text extraCSS ]
                    )
                , Element.el
                    [ Element.width height
                    , Element.height Element.fill
                    , Element.padding 2
                    , Element.clip
                    , Element.htmlAttribute (HtmlAttribute.class "lm-ui-button__icon")
                    , Background.color iconBg
                    , Border.solid
                    , Border.color
                        (if selected then
                            borderColorActive

                         else
                            uiColorBorder
                        )
                    , Border.widthEach
                        { right = 1
                        , top = 0
                        , bottom = 0
                        , left = 0
                        }
                    ]
                    (Element.html iconSvg)
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
