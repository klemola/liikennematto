module Data.Colors exposing
    ( darkBlue
    , darkBlueCSS
    , darkBrownCSS
    , darkGreen
    , darkGreenCSS
    , gray1
    , gray1CSS
    , gray2
    , gray2CSS
    , gray3
    , gray3CSS
    , gray4
    , gray4CSS
    , gray5
    , gray5CSS
    , gray6
    , gray6CSS
    , gray7
    , gray7CSS
    , lightBlue
    , lightBlueCSS
    , lightBrown
    , lightBrownCSS
    , lightGreen
    , lightGreenCSS
    , orange
    , orangeCSS
    , red
    , redCSS
    , uiCompat
    , withAlpha
    , yellow
    , yellowCSS
    , yellowDarker
    , yellowDarkerCSS
    , yellowGreen
    , yellowGreenCSS
    )

import Color
import Element


uiCompat : Color.Color -> Element.Color
uiCompat color =
    Color.toRgba color |> Element.fromRgb


withAlpha : Float -> Color.Color -> Color.Color
withAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba { rgba | alpha = alpha }


gray1 : Color.Color
gray1 =
    Color.rgb255 61 52 52


gray1CSS : String
gray1CSS =
    Color.toCssString gray1


gray2 : Color.Color
gray2 =
    Color.rgb255 118 101 101


gray2CSS : String
gray2CSS =
    Color.toCssString gray2


gray3 : Color.Color
gray3 =
    Color.rgb255 188 169 169


gray3CSS : String
gray3CSS =
    Color.toCssString gray3


gray4 : Color.Color
gray4 =
    Color.rgb255 218 209 209


gray4CSS : String
gray4CSS =
    Color.toCssString gray4


gray5 : Color.Color
gray5 =
    Color.rgb255 217 217 181


gray5CSS : String
gray5CSS =
    Color.toCssString gray5


gray6 : Color.Color
gray6 =
    Color.rgb255 240 240 221


gray6CSS : String
gray6CSS =
    Color.toCssString gray6


gray7 : Color.Color
gray7 =
    Color.rgb255 249 249 233


gray7CSS : String
gray7CSS =
    Color.toCssString gray7


red : Color.Color
red =
    Color.rgb255 223 63 63


redCSS : String
redCSS =
    Color.toCssString red


redDarker : Color.Color
redDarker =
    Color.rgb255 188 47 47


redDarkerCSS : String
redDarkerCSS =
    Color.toCssString red


orange : Color.Color
orange =
    Color.rgb255 229 140 77


orangeCSS : String
orangeCSS =
    Color.toCssString orange


orangeDarker : Color.Color
orangeDarker =
    Color.rgb255 233 134 63


orangeDarkerCSS : String
orangeDarkerCSS =
    Color.toCssString orangeDarker


yellow : Color.Color
yellow =
    Color.rgb255 235 210 82


yellowCSS : String
yellowCSS =
    Color.toCssString yellow


yellowDarker : Color.Color
yellowDarker =
    Color.rgb255 231 200 45


yellowDarkerCSS : String
yellowDarkerCSS =
    Color.toCssString yellowDarker


darkBrown : Color.Color
darkBrown =
    Color.rgb255 150 111 64


darkBrownCSS : String
darkBrownCSS =
    Color.toCssString darkBrown


darkBrownDarker : Color.Color
darkBrownDarker =
    Color.rgb255 134 100 60


darkBrownDarkerCSS : String
darkBrownDarkerCSS =
    Color.toCssString darkBrownDarker


lightBrown : Color.Color
lightBrown =
    Color.rgb255 203 164 118


lightBrownCSS : String
lightBrownCSS =
    Color.toCssString lightBrown


lightBrownDarker : Color.Color
lightBrownDarker =
    Color.rgb255 191 146 95


lightBrownDarkerCSS : String
lightBrownDarkerCSS =
    Color.toCssString lightBrownDarker


lightGreen : Color.Color
lightGreen =
    Color.rgb255 112 174 97


lightGreenCSS : String
lightGreenCSS =
    Color.toCssString lightGreen


lightGreenDarker : Color.Color
lightGreenDarker =
    Color.rgb255 92 152 78


lightGreenDarkerCSS : String
lightGreenDarkerCSS =
    Color.toCssString lightGreenDarker


darkGreen : Color.Color
darkGreen =
    Color.rgb255 76 137 67


darkGreenCSS : String
darkGreenCSS =
    Color.toCssString darkGreen


darkGreenDarker : Color.Color
darkGreenDarker =
    Color.rgb255 61 110 54


darkGreenDarkerCSS : String
darkGreenDarkerCSS =
    Color.toCssString darkGreenDarker


yellowGreen : Color.Color
yellowGreen =
    Color.rgb255 155 185 70


yellowGreenCSS : String
yellowGreenCSS =
    Color.toCssString yellowGreen


yellowGreenDarker : Color.Color
yellowGreenDarker =
    Color.rgb255 130 155 59


yellowGreenDarkerCSS : String
yellowGreenDarkerCSS =
    Color.toCssString yellowGreenDarker


darkBlue : Color.Color
darkBlue =
    Color.rgb255 49 140 231


darkBlueCSS : String
darkBlueCSS =
    Color.toCssString darkBlue


darkBlueDarker : Color.Color
darkBlueDarker =
    Color.rgb255 25 120 215


darkBlueDarkerCSS : String
darkBlueDarkerCSS =
    Color.toCssString darkBlueDarker


lightBlue : Color.Color
lightBlue =
    Color.rgb255 136 176 221


lightBlueCSS : String
lightBlueCSS =
    Color.toCssString lightBlue


lightBlueDarker : Color.Color
lightBlueDarker =
    Color.rgb255 104 155 212


lightBlueDarkerCSS : String
lightBlueDarkerCSS =
    Color.toCssString lightBlueDarker
