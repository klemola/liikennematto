module Render.Conversion exposing
    ( PixelsToMetersRatio
    , defaultPixelsToMetersRatio
    , pointToPixels
    , toMetersValue
    , toPixelsValue
    )

import Common exposing (GlobalCoordinates)
import Length exposing (Length)
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)


type alias PixelsToMetersRatio =
    Quantity Float (Rate Pixels.Pixels Length.Meters)


defaultPixelsToMetersRatio : PixelsToMetersRatio
defaultPixelsToMetersRatio =
    Pixels.pixels 6 |> Quantity.per (Length.meters 1)


toPixelsValue : PixelsToMetersRatio -> Length -> Float
toPixelsValue pixelsToMetersRatio length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


pointToPixels : PixelsToMetersRatio -> Point2d Length.Meters GlobalCoordinates -> { x : Float, y : Float }
pointToPixels pixelsToMetersRatio point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels


toMetersValue : PixelsToMetersRatio -> Float -> Length
toMetersValue pixelsToMetersRatio pixels =
    pixels
        |> Pixels.pixels
        |> Quantity.at_ pixelsToMetersRatio
