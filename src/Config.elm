module Config exposing
    ( boardSize
    , boardSizeScaled
    , boardSizeScaledInMeters
    , borderRadius
    , borderSize
    , colors
    , pixelsToMeters
    , pixelsToMetersRatio
    , tileSize
    , tileSizeInMeters
    , uiDimensions
    , whitespace
    )

import Element exposing (rgb255, rgba255)
import Length exposing (Length)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)



--
-- Unitless constants
--


boardSize : Int
boardSize =
    10



--
-- Pixels x meters conversion
--


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 5 |> Quantity.per (Length.meters 1)


pixelsToMeters : Float -> Length
pixelsToMeters pixels =
    Pixels.float pixels
        |> Quantity.at_ pixelsToMetersRatio



-- Room for improvement: move tile and board size constants to the Board module, merge Cell into it as well


tileSize : Quantity Float Pixels
tileSize =
    Pixels.float 80


boardSizeScaled : Quantity Int Pixels
boardSizeScaled =
    tileSize
        |> Quantity.floor
        |> Quantity.multiplyBy boardSize


tileSizeInMeters : Length
tileSizeInMeters =
    tileSize
        |> Quantity.at_ pixelsToMetersRatio


boardSizeScaledInMeters : Length
boardSizeScaledInMeters =
    boardSizeScaled
        |> Quantity.toFloatQuantity
        |> Quantity.at_ pixelsToMetersRatio



--
-- Visuals
--


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
    { mainBackground = rgb255 88 135 140
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
