module UI.Core exposing
    ( InputKind(..)
    , borderRadiusPanel
    , borderSize
    , cellHighlightWidth
    , colorBorder
    , colorCardBackground
    , colorDanger
    , colorErrorScreenBackground
    , colorMainBackground
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
    , overlayId
    , renderSafeAreaXSize
    , renderSafeAreaYSize
    , scrollbarAwareOffsetF
    , textSize
    , whitespaceRegular
    , whitespaceTight
    , zoomControlWidth
    , zoomTrackHeight
    , zoomTrackWidth
    )

import Data.Colors as Colors exposing (uiCompat)
import Element


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


borderRadiusPanel : Int
borderRadiusPanel =
    15


scrollbarAwareOffset : Int
scrollbarAwareOffset =
    18


scrollbarAwareOffsetF : Float
scrollbarAwareOffsetF =
    toFloat scrollbarAwareOffset


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
