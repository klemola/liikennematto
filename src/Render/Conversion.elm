module Render.Conversion exposing
    ( metersToViewBoxRatio
    , pixelsToMetersRatio
    , pointToPixels
    , toMetersValue
    , toPixelsValue
    , toViewBoxValue
    )

import Length exposing (Length)
import Model.Geometry exposing (LMPoint2d)
import Pixels
import Point2d
import Quantity exposing (Quantity, Rate)


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 6 |> Quantity.per (Length.meters 1)


metersToViewBoxRatio : Quantity Float (Rate Length.Meters Pixels.Pixels)
metersToViewBoxRatio =
    Length.meters 1 |> Quantity.per (Pixels.pixels 16)


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


toViewBoxValue : Length -> Float
toViewBoxValue length =
    length
        |> Quantity.at_ metersToViewBoxRatio
        |> Pixels.inPixels


pointToPixels : LMPoint2d -> { x : Float, y : Float }
pointToPixels point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels


toMetersValue : Float -> Length
toMetersValue pixels =
    pixels
        |> Pixels.pixels
        |> Quantity.at_ pixelsToMetersRatio
