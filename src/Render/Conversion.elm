module Render.Conversion exposing
    ( PixelsToMetersRatio
    , defaultPixelsToMetersRatio
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


type alias PixelsToMetersRatio =
    Quantity Float (Rate Pixels.Pixels Length.Meters)


type alias MetersToViewboxRatio =
    Quantity Float (Rate Length.Meters Pixels.Pixels)


defaultPixelsToMetersRatio : PixelsToMetersRatio
defaultPixelsToMetersRatio =
    Pixels.pixels 6 |> Quantity.per (Length.meters 1)


metersToViewBoxRatio : MetersToViewboxRatio
metersToViewBoxRatio =
    Length.meters 1 |> Quantity.per (Pixels.pixels 16)


toPixelsValue : PixelsToMetersRatio -> Length -> Float
toPixelsValue pixelsToMetersRatio length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


toViewBoxValue : Length -> Float
toViewBoxValue length =
    length
        |> Quantity.at_ metersToViewBoxRatio
        |> Pixels.inPixels


pointToPixels : PixelsToMetersRatio -> LMPoint2d -> { x : Float, y : Float }
pointToPixels pixelsToMetersRatio point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels


toMetersValue : PixelsToMetersRatio -> Float -> Length
toMetersValue pixelsToMetersRatio pixels =
    pixels
        |> Pixels.pixels
        |> Quantity.at_ pixelsToMetersRatio
