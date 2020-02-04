module Config exposing (..)

import Common exposing (Coords)


boardSize =
    8


blockSize =
    64


roads : List Coords
roads =
    [ ( 1, 1 )
    , ( 2, 1 )
    , ( 3, 1 )
    , ( 4, 1 )
    , ( 5, 1 )
    , ( 5, 2 )
    , ( 5, 3 )
    , ( 5, 4 )
    , ( 5, 5 )
    , ( 6, 5 )
    , ( 7, 5 )
    , ( 8, 5 )
    ]
