module BitMask exposing
    ( fourBitValue
    , isSurroundedByEmptyTiles
    )


type alias ParallelNeighbors =
    { north : Bool
    , west : Bool
    , east : Bool
    , south : Bool
    }


{-| Calculates tile number (ID) based on surrounding tiles

    North = 2^0 = 1
    West = 2^1 = 2
    East = 2^2 = 4
    South = 2^3 = 8

    e.g. tile bordered by tiles in north and east directions 1*1 + 2*0 + 4*1 + 8*0 = 0101 = 5

-}
fourBitValue : ParallelNeighbors -> Int
fourBitValue { north, west, east, south } =
    1 * boolToBinary north + 2 * boolToBinary west + 4 * boolToBinary east + 8 * boolToBinary south


boolToBinary : Bool -> Int
boolToBinary booleanValue =
    if booleanValue then
        1

    else
        0


isSurroundedByEmptyTiles : Int -> Bool
isSurroundedByEmptyTiles value =
    value == 0
