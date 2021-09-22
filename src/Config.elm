module Config exposing
    ( boardSize
    , boardSizeScaled
    , boardSizeScaledInMeters
    , tileSize
    , tileSizeInMeters
    )

import Length exposing (Length)
import Model.Geometry exposing (pixelsToMetersRatio)
import Pixels exposing (Pixels)
import Quantity exposing (Quantity)



--
-- Unitless constants
--


boardSize : Int
boardSize =
    10



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
