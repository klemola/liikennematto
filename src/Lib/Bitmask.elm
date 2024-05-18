module Lib.Bitmask exposing (OrthogonalNeighbors, fiveBitMask, fourBitMask)


type alias OrthogonalNeighbors =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    }


{-| Calculates tile ID based on surrounding tiles

    Up = 2^0 = 1
    Left = 2^1 = 2
    Right = 2^2 = 4
    Down = 2^3 = 8

    e.g. tile bordered by tiles in Up, Left and Right directions

    1*1 + 2*1 + 4*1 + 8*0  = 111 = 7

-}
fourBitMask : OrthogonalNeighbors -> Int
fourBitMask { up, left, right, down } =
    -- 1 * up
    boolToBinary up
        + (2 * boolToBinary left)
        + (4 * boolToBinary right)
        + (8 * boolToBinary down)


{-| Calculates tile Id like the four-bit variant, with extra modifier to allow two variants per ID
-}
fiveBitMask : OrthogonalNeighbors -> Bool -> Int
fiveBitMask { up, left, right, down } modifier =
    -- 1 * up
    boolToBinary up
        + (2 * boolToBinary left)
        + (4 * boolToBinary right)
        + (8 * boolToBinary down)
        + (16 * boolToBinary modifier)


boolToBinary : Bool -> Int
boolToBinary booleanValue =
    if booleanValue then
        1

    else
        0
