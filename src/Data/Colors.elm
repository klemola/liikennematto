module Data.Colors exposing (..)

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
    Color.rgb255 48 37 37


gray1CSS : String
gray1CSS =
    Color.toCssString gray1


gray2 : Color.Color
gray2 =
    Color.rgb255 120 94 94


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
    Color.rgb255 240 240 221


gray5CSS : String
gray5CSS =
    Color.toCssString gray5


gray6 : Color.Color
gray6 =
    Color.rgb255 249 249 233


gray6CSS : String
gray6CSS =
    Color.toCssString gray6


red : Color.Color
red =
    Color.rgb255 223 63 63


redCSS : String
redCSS =
    Color.toCssString red


orange : Color.Color
orange =
    Color.rgb255 235 145 81


orangeCSS : String
orangeCSS =
    Color.toCssString orange


yellow : Color.Color
yellow =
    Color.rgb255 235 210 82


yellowCSS : String
yellowCSS =
    Color.toCssString yellow


darkBrown : Color.Color
darkBrown =
    Color.rgb255 150 111 64


darkBrownCSS : String
darkBrownCSS =
    Color.toCssString darkBrown


lightBrown : Color.Color
lightBrown =
    Color.rgb255 216 164 106


lightBrownCSS : String
lightBrownCSS =
    Color.toCssString lightBrown


lightGreen : Color.Color
lightGreen =
    Color.rgb255 112 174 97


lightGreenCSS : String
lightGreenCSS =
    Color.toCssString lightGreen


darkGreen : Color.Color
darkGreen =
    Color.rgb255 76 137 67


darkGreenCSS : String
darkGreenCSS =
    Color.toCssString darkGreen


yellowGreen : Color.Color
yellowGreen =
    Color.rgb255 155 185 70


yellowGreenCSS : String
yellowGreenCSS =
    Color.toCssString yellowGreen


darkBlue : Color.Color
darkBlue =
    Color.rgb255 49 140 231


darkBlueCSS : String
darkBlueCSS =
    Color.toCssString darkBlue


lightBlue : Color.Color
lightBlue =
    Color.rgb255 136 176 221


lightBlueCSS : String
lightBlueCSS =
    Color.toCssString lightBlue
