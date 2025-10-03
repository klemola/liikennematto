module UI.Core exposing
    ( InputKind(..)
    , borderSize
    , cellHighlightWidth
    , colorDanger
    , colorMainBackground
    , colorNotAllowed
    , colorRenderEdge
    , colorTarget
    , colorTransparent
    , containerId
    , overlayId
    , renderSafeAreaXSize
    , renderSafeAreaYSize
    , scrollbarAwareOffsetF
    , uiColorBorder
    , uiColorText
    , whitespaceRegular
    , whitespaceTight
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


cellHighlightWidth : Int
cellHighlightWidth =
    3



--
-- UI colors
--


colorMainBackground : Element.Color
colorMainBackground =
    uiCompat Colors.lightGreen


uiColorText : Element.Color
uiColorText =
    Element.rgb255 40 52 72


uiColorBorder : Element.Color
uiColorBorder =
    Element.rgb255 105 129 171


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
