module Render.Conversion exposing (pixelsToMetersRatio, toPixelsValue)

import Length exposing (Length)
import Pixels
import Quantity exposing (Quantity, Rate)


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 8 |> Quantity.per (Length.meters 1)


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels
