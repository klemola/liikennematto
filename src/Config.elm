module Config exposing (..)

import Element exposing (rgb255, rgba255)


boardSize : Int
boardSize =
    10


tileSize : Float
tileSize =
    80


innerLaneOffset : Float
innerLaneOffset =
    26


outerLaneOffset : Float
outerLaneOffset =
    54


carLength : Float
carLength =
    24


carWidth : Float
carWidth =
    12


nodeSize : Float
nodeSize =
    4


boardSizeScaled : Float
boardSizeScaled =
    toFloat boardSize * tileSize


environmentUpdateFrequency : Float
environmentUpdateFrequency =
    1000


dequeueFrequency : Float
dequeueFrequency =
    500


type alias UiDimensions =
    { toolbar : Int
    , controlButtonS : Int
    , controlButtonL : Int
    , panel : Int
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
    , overlay = 256
    , text = 14
    , smallControlsBreakpoint = 720
    }


colors =
    { mainBackground = rgb255 68 115 120
    , menuBackground = rgb255 159 192 198
    , buttonBackground = rgb255 228 228 235
    , listItemBackground = rgb255 109 151 156
    , text = rgb255 52 65 67
    , textInverse = rgb255 222 222 222
    , link = rgb255 10 132 199
    , selected = rgb255 242 212 13
    , danger = rgb255 235 119 52
    , notAllowed = rgb255 245 66 84
    , target = rgb255 222 222 222
    , terrain = rgb255 33 191 154
    , transparent = rgba255 0 0 0 0
    , lightBorder = rgb255 220 220 226
    , heavyBorder = rgb255 53 93 97
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
