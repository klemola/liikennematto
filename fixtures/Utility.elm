module Utility exposing (..)

import Common
import Config exposing (pixelsToMetersRatio)
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d)
import Pixels
import Point2d
import Quantity


toLMPoint2d : Float -> Float -> LMPoint2d
toLMPoint2d pixelsX pixelsY =
    Point2d.xy
        (Pixels.float pixelsX |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float pixelsY |> Quantity.at_ pixelsToMetersRatio)


createBoundingBox : ( Float, Float ) -> Float -> Float -> LMBoundingBox2d
createBoundingBox ( x, y ) width height =
    Common.boundingBoxWithDimensions
        (Pixels.float width |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float height |> Quantity.at_ pixelsToMetersRatio)
        (toLMPoint2d x y)
