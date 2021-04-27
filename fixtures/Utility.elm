module Utility exposing (..)

import Config exposing (pixelsToMetersRatio)
import Geometry
import Pixels
import Point2d
import Quantity


toLMPoint2d : Float -> Float -> Geometry.LMPoint2d
toLMPoint2d pixelsX pixelsY =
    Point2d.xy
        (Pixels.float pixelsX |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float pixelsY |> Quantity.at_ pixelsToMetersRatio)


createBoundingBox : ( Float, Float ) -> Float -> Float -> Geometry.LMBoundingBox2d
createBoundingBox ( x, y ) width height =
    Geometry.boundingBoxWithDimensions
        (Pixels.float width |> Quantity.at_ pixelsToMetersRatio)
        (Pixels.float height |> Quantity.at_ pixelsToMetersRatio)
        (toLMPoint2d x y)
