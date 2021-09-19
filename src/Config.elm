module Config exposing
    ( boardSize
    , boardSizeScaled
    , boardSizeScaledInMeters
    , pixelsToMeters
    , pixelsToMetersRatio
    , tileSize
    , tileSizeInMeters
    )

import Length exposing (Length)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity, Rate)



--
-- Unitless constants
--


boardSize : Int
boardSize =
    10



--
-- Pixels x meters conversion
--


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 5 |> Quantity.per (Length.meters 1)


pixelsToMeters : Float -> Length
pixelsToMeters pixels =
    Pixels.float pixels
        |> Quantity.at_ pixelsToMetersRatio



--
-- Tilemap
--


tileSize : Quantity Float Pixels
tileSize =
    Pixels.float 80


boardSizeScaled : Quantity Int Pixels
boardSizeScaled =
    tileSize
        |> Quantity.floor
        |> Quantity.multiplyBy boardSize


tileSizeInMeters : Length
tileSizeInMeters =
    tileSize
        |> Quantity.at_ pixelsToMetersRatio


boardSizeScaledInMeters : Length
boardSizeScaledInMeters =
    boardSizeScaled
        |> Quantity.toFloatQuantity
        |> Quantity.at_ pixelsToMetersRatio
