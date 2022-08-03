module Render.Conversion exposing (pixelsToMetersRatio, pointToPixels, toPixelsValue)

import Length exposing (Length)
import Model.Geometry exposing (LMPoint2d)
import Pixels
import Point2d
import Quantity exposing (Quantity, Rate)


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 8 |> Quantity.per (Length.meters 1)


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


pointToPixels : LMPoint2d -> { x : Float, y : Float }
pointToPixels point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels
