module UI.Core exposing
    ( ControlButtonProperties
    , ControlButtonSize(..)
    , borderRadiusButton
    , borderRadiusPanel
    , borderSize
    , cellHighlightWidth
    , colorBorder
    , colorCardBackground
    , colorDanger
    , colorErrorScreenBackground
    , colorMainBackground
    , colorMenuBackground
    , colorMenuBackgroundInverse
    , colorNotAllowed
    , colorRenderEdge
    , colorSplashScreenBackground
    , colorTarget
    , colorText
    , colorTextInverse
    , colorTransparent
    , colorZoomStepGuide
    , colorZoomThumbBackground
    , colorZoomTrackBackground
    , containerId
    , controlButton
    , overlayId
    , renderSafeAreaXSize
    , renderSafeAreaYSize
    , scrollbarAwareOffsetF
    , smallControlButton
    , textSize
    , whitespaceRegular
    , whitespaceTight
    , zoomControlWidth
    , zoomTrackHeight
    , zoomTrackWidth
    )

import Data.Colors as Colors exposing (uiCompat)
import Data.Icons exposing (IconKind, chooseIcon)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


containerId : String
containerId =
    "liikennematto"


overlayId : String
overlayId =
    "lm-overlay"



--
-- Size and spacing
--


baseSpacing : Int
baseSpacing =
    20


whitespaceRegular : Int
whitespaceRegular =
    10


whitespaceTight : Int
whitespaceTight =
    5


borderSize : Int
borderSize =
    2


borderRadiusButton : Int
borderRadiusButton =
    10


borderRadiusPanel : Int
borderRadiusPanel =
    15


scrollbarAwareOffset : Int
scrollbarAwareOffset =
    baseSpacing


scrollbarAwareOffsetF : Float
scrollbarAwareOffsetF =
    toFloat scrollbarAwareOffset


controlButtonSize : Int
controlButtonSize =
    baseSpacing * 3


renderSafeAreaXSize : Int
renderSafeAreaXSize =
    baseSpacing * 4 + scrollbarAwareOffset


renderSafeAreaYSize : Int
renderSafeAreaYSize =
    baseSpacing * 4 + scrollbarAwareOffset


textSize : Int
textSize =
    14


zoomControlWidth : Int
zoomControlWidth =
    baseSpacing + (whitespaceTight * 2)


zoomTrackHeight : Int
zoomTrackHeight =
    baseSpacing * 8


zoomTrackWidth : Int
zoomTrackWidth =
    14


cellHighlightWidth : Int
cellHighlightWidth =
    3



--
-- UI colors
--


colorSplashScreenBackground : Element.Color
colorSplashScreenBackground =
    uiCompat Colors.gray7


colorErrorScreenBackground : Element.Color
colorErrorScreenBackground =
    uiCompat Colors.red


colorMainBackground : Element.Color
colorMainBackground =
    uiCompat Colors.lightGreen


colorMenuBackground : Element.Color
colorMenuBackground =
    uiCompat (Colors.withAlpha 0.35 Colors.gray7)


colorMenuBackgroundInverse : Element.Color
colorMenuBackgroundInverse =
    uiCompat (Colors.withAlpha 0.35 Colors.gray1)


colorCardBackground : Element.Color
colorCardBackground =
    uiCompat Colors.gray6


colorText : Element.Color
colorText =
    uiCompat Colors.gray2


colorTextInverse : Element.Color
colorTextInverse =
    uiCompat Colors.gray7


colorSelected : Element.Color
colorSelected =
    uiCompat Colors.gray7


colorDanger : Element.Color
colorDanger =
    uiCompat (Colors.withAlpha 0.65 Colors.yellow)


colorNotAllowed : Element.Color
colorNotAllowed =
    uiCompat (Colors.withAlpha 0.65 Colors.red)


colorTarget : Element.Color
colorTarget =
    uiCompat (Colors.withAlpha 0.65 Colors.gray7)


colorTransparent : Element.Color
colorTransparent =
    Element.rgba255 0 0 0 0


colorRenderEdge : Element.Color
colorRenderEdge =
    uiCompat Colors.darkGreen


colorBorder : Element.Color
colorBorder =
    uiCompat Colors.gray1


colorZoomTrackBackground : Element.Color
colorZoomTrackBackground =
    uiCompat Colors.darkBlue


colorZoomThumbBackground : Element.Color
colorZoomThumbBackground =
    uiCompat Colors.gray6


colorZoomStepGuide : Element.Color
colorZoomStepGuide =
    uiCompat Colors.gray6



--
-- Control buttons
--


type alias ControlButtonProperties msg =
    { iconKind : IconKind
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
buildControlButton size { iconKind, onPress, selected, disabled } =
    let
        baseSize =
            case size of
                Small ->
                    controlButtonSize // 2

                Large ->
                    controlButtonSize

        buttonSize =
            Element.px (baseSize - (2 * borderSize))

        alpha =
            if disabled then
                0.5

            else
                1

        ( iconHtml, backgroundColor, activeBackgroundColor ) =
            chooseIcon iconKind
    in
    Input.button
        [ Background.color (uiCompat backgroundColor)
        , Element.width buttonSize
        , Element.height buttonSize
        , Element.alpha alpha
        , Element.clip
        , Element.mouseOver
            [ Background.color (uiCompat activeBackgroundColor) ]
        , Element.mouseDown
            [ Background.color (uiCompat activeBackgroundColor) ]
        , Border.width borderSize
        , Border.rounded
            (if size == Small then
                borderRadiusButton // 2

             else
                borderRadiusButton
            )
        , Border.solid
        , Border.color
            (if selected then
                colorSelected

             else
                colorBorder
            )
        ]
        { onPress =
            if disabled then
                Nothing

            else
                Just onPress
        , label = Element.html iconHtml
        }
