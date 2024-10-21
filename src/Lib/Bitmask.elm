module Lib.Bitmask exposing (OrthogonalMatch, fiveBitMask, fourBitMask, mergeMatches)


type alias OrthogonalMatch =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    }


mergeMatches : OrthogonalMatch -> OrthogonalMatch -> OrthogonalMatch
mergeMatches n1 n2 =
    { up = n1.up || n2.up
    , left = n1.left || n2.left
    , right = n1.right || n2.right
    , down = n1.down || n2.down
    }


{-| Calculates tile ID based on surrounding tiles

    Up = 2^0 = 1
    Left = 2^1 = 2
    Right = 2^2 = 4
    Down = 2^3 = 8

    e.g. tile bordered by tiles in Up, Left and Right directions

    1*1 + 2*1 + 4*1 + 8*0  = 111 = 7

-}
fourBitMask : OrthogonalMatch -> Int
fourBitMask { up, left, right, down } =
    -- 1 * up
    boolToBinary up
        + (2 * boolToBinary left)
        + (4 * boolToBinary right)
        + (8 * boolToBinary down)


{-| Calculates tile Id like the four-bit variant, with extra modifier to allow two variants per ID
-}
fiveBitMask : OrthogonalMatch -> Bool -> Int
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
