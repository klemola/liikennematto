module Render.Conversion exposing
    ( metersToViewBoxRatio
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


metersToViewBoxRatio : Quantity Float (Rate Length.Meters Pixels.Pixels)
metersToViewBoxRatio =
    Length.meters 1 |> Quantity.per (Pixels.pixels 16)


toPixelsValue : Quantity Float (Rate Pixels.Pixels Length.Meters) -> Length -> Float
toPixelsValue pixelsToMetersRatio length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


toViewBoxValue : Length -> Float
toViewBoxValue length =
    length
        |> Quantity.at_ metersToViewBoxRatio
        |> Pixels.inPixels


pointToPixels : Quantity Float (Rate Pixels.Pixels Length.Meters) -> LMPoint2d -> { x : Float, y : Float }
pointToPixels pixelsToMetersRatio point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels


toMetersValue : Quantity Float (Rate Pixels.Pixels Length.Meters) -> Float -> Length
toMetersValue pixelsToMetersRatio pixels =
    pixels
        |> Pixels.pixels
        |> Quantity.at_ pixelsToMetersRatio
