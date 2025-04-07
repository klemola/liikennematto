module UI.Core exposing
    ( ControlButtonConfig
    , ControlButtonContent(..)
    , ControlButtonSize(..)
    , InputEvent
    , InputKind(..)
    , ZoomLevel(..)
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
    , textSize
    , textSizeMini
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
import Html
import Tilemap.Cell exposing (Cell)


type ZoomLevel
    = Near
    | Far
    | VeryFar


type alias InputEvent =
    { cell : Cell
    , kind : InputKind
    }


type InputKind
    = Primary
    | Secondary


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


textSizeMini : Int
textSizeMini =
    12


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
    uiCompat (Colors.withAlpha 0.25 Colors.yellow)


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


type ControlButtonContent
    = Icon IconKind
    | Text String


type alias ControlButtonConfig msg =
    { content : ControlButtonContent
    , onPress : msg
    , selected : Bool
    , disabled : Bool
    , size : ControlButtonSize
    }


type ControlButtonSize
    = Large
    | Small
    | FitToContent


controlButton : ControlButtonConfig msg -> Element msg
controlButton =
    buildControlButton


buttonSize baseSize =
    Element.px (baseSize - (2 * borderSize))


buildControlButton : ControlButtonConfig msg -> Element msg
buildControlButton { content, onPress, selected, disabled, size } =
    let
        ( width, height, padding ) =
            case size of
                Small ->
                    ( buttonSize (controlButtonSize // 2), buttonSize (controlButtonSize // 2), 0 )

                Large ->
                    ( buttonSize controlButtonSize, buttonSize controlButtonSize, 0 )

                FitToContent ->
                    ( Element.fill, Element.fill, 4 )

        alpha =
            if disabled then
                0.5

            else
                1

        ( labelHtml, backgroundColor, activeBackgroundColor ) =
            case content of
                Icon iconKind ->
                    chooseIcon iconKind

                Text textContent ->
                    ( Html.text textContent, Colors.gray5, Colors.gray1 )
    in
    Input.button
        [ Background.color (uiCompat backgroundColor)
        , Element.width width
        , Element.height height
        , Element.padding padding
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
        , label = Element.html labelHtml
        }
