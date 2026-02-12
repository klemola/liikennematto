module Render.Conversion exposing
    ( PixelsToMetersRatio
    , defaultPixelsToMetersRatio
    , pointToPixels
    , tileSizePixels
    , toMetersValue
    , toPixelsValue
    )

import Common exposing (GlobalCoordinates)
import Length exposing (Length)
import Pixels
import Point2d exposing (Point2d)
import Quantity exposing (Quantity, Rate)
import Tilemap.Cell as Cell


type alias PixelsToMetersRatio =
    Quantity Float (Rate Pixels.Pixels Length.Meters)


defaultPixelsToMetersRatio : PixelsToMetersRatio
defaultPixelsToMetersRatio =
    Pixels.pixels 16 |> Quantity.per (Length.meters 1)


tileSizePixels : Float
tileSizePixels =
    toPixelsValue defaultPixelsToMetersRatio Cell.size


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
