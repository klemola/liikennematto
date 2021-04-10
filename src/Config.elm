module Config exposing
    ( acceleration
    , boardSize
    , boardSizeScaled
    , boardSizeScaledInMeters
    , borderRadius
    , borderSize
    , carCollisionCircleRadius
    , carFieldOfView
    , carLength
    , carProximityCutoff
    , carRotationTolerance
    , carWidth
    , colors
    , dequeueFrequency
    , environmentUpdateFrequency
    , innerLaneOffset
    , lotExitOffset
    , maxVelocity
    , nodeSize
    , outerLaneOffset
    , overlapThreshold
    , pixelsToMeters
    , pixelsToMetersRatio
    , tileSize
    , tileSizeInMeters
    , trafficLightRadius
    , trafficLightReactionDistance
    , trafficLightsStopMargin
    , uTurnDistance
    , uiDimensions
    , whitespace
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Element exposing (rgb255, rgba255)
import Length exposing (Length)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed)



--
-- Unitless constants
--


environmentUpdateFrequency : Float
environmentUpdateFrequency =
    1000


dequeueFrequency : Float
dequeueFrequency =
    500


boardSize : Int
boardSize =
    10



--
-- Unit constants
--


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 5 |> Quantity.per (Length.meters 1)


pixelsToMeters : Float -> Length
pixelsToMeters pixels =
    Pixels.float pixels
        |> Quantity.at_ pixelsToMetersRatio


tileSize : Quantity Float Pixels
tileSize =
    Pixels.float 80


boardSizeScaled : Quantity Int Pixels
boardSizeScaled =
    tileSize
        |> Quantity.floor
        |> Quantity.multiplyBy boardSize


nodeSize : Quantity Float Pixels
nodeSize =
    Pixels.float 4


trafficLightRadius : Quantity Float Pixels
trafficLightRadius =
    Pixels.float 5


tileSizeInMeters : Length
tileSizeInMeters =
    tileSize
        |> Quantity.at_ pixelsToMetersRatio


boardSizeScaledInMeters : Length
boardSizeScaledInMeters =
    boardSizeScaled
        |> Quantity.toFloatQuantity
        |> Quantity.at_ pixelsToMetersRatio


carLength : Length
carLength =
    pixelsToMeters 24


carWidth : Length
carWidth =
    pixelsToMeters 12


innerLaneOffset : Length
innerLaneOffset =
    pixelsToMeters 26


outerLaneOffset : Length
outerLaneOffset =
    pixelsToMeters 54


maxVelocity : Speed
maxVelocity =
    Speed.metersPerSecond 11.1


carRotationTolerance : Angle
carRotationTolerance =
    Angle.degrees 5


carFieldOfView : Angle
carFieldOfView =
    Angle.degrees 45


acceleration :
    { speedUp : Acceleration
    , breakingSlow : Acceleration
    , breakingFast : Acceleration
    }
acceleration =
    { speedUp = Acceleration.metersPerSecondSquared 5
    , breakingSlow = Acceleration.metersPerSecondSquared -10
    , breakingFast = Acceleration.metersPerSecondSquared -40
    }


overlapThreshold : Length
overlapThreshold =
    Length.meters 0.1


carProximityCutoff : Length
carProximityCutoff =
    tileSizeInMeters


carCollisionCircleRadius : Length
carCollisionCircleRadius =
    carWidth
        |> Quantity.divideBy 1.5


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


lotExitOffset : Length
lotExitOffset =
    Length.meters 8


trafficLightReactionDistance : Length
trafficLightReactionDistance =
    Length.meters 50


trafficLightsStopMargin : Length
trafficLightsStopMargin =
    Length.meters 5



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
