module UI.Button exposing
    ( ButtonConfig
    , iconButton
    , iconButtonLarge
    , iconWithTextButton
    , textButton
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Svg exposing (Svg)
import UI.Core exposing (whitespaceTight)


borderRadiusPx : Int
borderRadiusPx =
    10


borderSizePx : Int
borderSizePx =
    2


buttonHeightPx : Int
buttonHeightPx =
    48


buttonHeightSmallPx : Int
buttonHeightSmallPx =
    28


buttonHeightLargePx : Int
buttonHeightLargePx =
    64


type alias ButtonConfig msg =
    { onPress : msg
    , selected : Bool
    , disabled : Bool
    }


bgColor =
    Element.rgb255 239 241 245


bgColorActive =
    Element.rgb255 253 253 247


borderColor =
    Element.rgb255 112 131 164


borderColorActive =
    Element.rgb255 64 133 201


contentColor =
    Element.rgb255 44 53 68


buttonSize baseSize =
    Element.px (baseSize - (2 * borderSizePx))


iconButton : ButtonConfig msg -> Svg msg -> Element msg
iconButton =
    createIconButton buttonHeightPx


iconButtonLarge : ButtonConfig msg -> Svg msg -> Element msg
iconButtonLarge =
    createIconButton buttonHeightLargePx


createIconButton : Int -> ButtonConfig msg -> Svg msg -> Element msg
createIconButton buttonSizePx { onPress, selected, disabled } icon =
    let
        width =
            buttonSize buttonSizePx

        height =
            buttonSize buttonSizePx

        alpha =
            if disabled then
                0.5

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
                borderColor
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
            buttonSize buttonHeightPx

        alpha =
            if disabled then
                0.5

            else
                1
    in
    Input.button
        [ Background.color bgColor
        , Element.width Element.fill
        , Element.height height
        , Element.padding 0
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver
            [ Background.color bgColorActive ]
        , Element.mouseDown
            [ Background.color bgColorActive ]
        , Font.bold
        , Font.size 14
        , Font.color contentColor
        , Border.width 1
        , Border.rounded borderRadiusPx
        , Border.solid
        , Border.color
            (if selected then
                borderColorActive

             else
                borderColor
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label =
            Element.row
                [ Element.spacing whitespaceTight
                ]
                [ Element.el
                    [ Element.width (Element.px 80)
                    , Element.height (Element.px 80)
                    ]
                    (Element.html icon)
                , Element.el []
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
        , Font.size 12
        , Font.color contentColor
        , Border.width borderSizePx
        , Border.rounded borderRadiusPx
        , Border.solid
        , Border.color
            (if selected then
                borderColorActive

             else
                borderColor
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = Element.text textContent
        }



-- ( width, height, padding, content ) =
--             case kind of
--                 IconAndText content ->
--                     ( buttonSize (controlButtonSize // 2)
--                     , buttonSize (controlButtonSize // 2)
--                     , 0
--                     , content
--                     )
--                 IconAndText content ->
--                     ( buttonSize (controlButtonSize // 2)
--                     , buttonSize (controlButtonSize // 2)
--                     , 0
--                     , content
--                     )
--                 IconOnly ->
--                     ( buttonSize controlButtonSize
--                     , buttonSize controlButtonSize
--                     , 0
--                     , ""
--                     )
