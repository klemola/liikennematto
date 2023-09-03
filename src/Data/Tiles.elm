module Data.Tiles exposing
    ( allTiles
    , defaultTile
    , pairingsForSocket
    )

import Model.Tile exposing (Socket(..), TileMeta)


pairingsForSocket : Socket -> List Socket
pairingsForSocket socket =
    case socket of
        Red ->
            [ Red, Pink, Yellow, Blue ]

        Green ->
            [ Green ]

        Blue ->
            [ Red, Yellow ]

        Yellow ->
            [ Red, Yellow ]

        Pink ->
            [ Red ]


allTiles : List TileMeta
allTiles =
    [ grass

    -- , loneRoad
    , horizontalRoad
    , verticalRoad
    , deadendUp
    , deadendDown
    , deadendRight
    , deadendLeft
    , curveBottomRight
    , curveBottomLeft
    , curveTopRight
    , curveTopLeft
    , intersectionTUp
    , intersectionTLeft
    , intersectionTRight
    , intersectionTDown
    , intersectionCross
    ]


defaultTile : TileMeta
defaultTile =
    grass


grass : TileMeta
grass =
    { id = 0
    , sockets =
        { top = Green
        , right = Green
        , bottom = Green
        , left = Green
        }
    }


loneRoad : TileMeta
loneRoad =
    { id = 17
    , sockets =
        { top = Green
        , right = Green
        , bottom = Green
        , left = Green
        }
    }


horizontalRoad : TileMeta
horizontalRoad =
    { id = 6
    , sockets =
        { top = Green
        , right = Red
        , bottom = Green
        , left = Red
        }
    }


verticalRoad : TileMeta
verticalRoad =
    { id = 9
    , sockets =
        { top = Red
        , right = Green
        , bottom = Red
        , left = Green
        }
    }


deadendDown : TileMeta
deadendDown =
    { id = 1
    , sockets =
        { top = Blue
        , right = Green
        , bottom = Green
        , left = Green
        }
    }


deadendRight : TileMeta
deadendRight =
    { id = 2
    , sockets =
        { top = Green
        , right = Green
        , bottom = Green
        , left = Blue
        }
    }


deadendLeft : TileMeta
deadendLeft =
    { id = 4
    , sockets =
        { top = Green
        , right = Blue
        , bottom = Green
        , left = Green
        }
    }


deadendUp : TileMeta
deadendUp =
    { id = 8
    , sockets =
        { top = Green
        , right = Green
        , bottom = Blue
        , left = Green
        }
    }


curveBottomRight : TileMeta
curveBottomRight =
    { id = 3
    , sockets =
        { top = Yellow
        , right = Green
        , bottom = Green
        , left = Yellow
        }
    }


curveBottomLeft : TileMeta
curveBottomLeft =
    { id = 5
    , sockets =
        { top = Yellow
        , right = Yellow
        , bottom = Green
        , left = Green
        }
    }


curveTopRight : TileMeta
curveTopRight =
    { id = 10
    , sockets =
        { top = Green
        , right = Green
        , bottom = Yellow
        , left = Yellow
        }
    }


curveTopLeft : TileMeta
curveTopLeft =
    { id = 12
    , sockets =
        { top = Green
        , right = Yellow
        , bottom = Yellow
        , left = Green
        }
    }


intersectionTUp : TileMeta
intersectionTUp =
    { id = 7
    , sockets =
        { top = Pink
        , right = Yellow
        , bottom = Green
        , left = Pink
        }
    }


intersectionTLeft : TileMeta
intersectionTLeft =
    { id = 11
    , sockets =
        { top = Pink
        , right = Green
        , bottom = Yellow
        , left = Pink
        }
    }


intersectionTRight : TileMeta
intersectionTRight =
    { id = 13
    , sockets =
        { top = Yellow
        , right = Pink
        , bottom = Pink
        , left = Green
        }
    }


intersectionTDown : TileMeta
intersectionTDown =
    { id = 14
    , sockets =
        { top = Green
        , right = Pink
        , bottom = Pink
        , left = Yellow
        }
    }


intersectionCross : TileMeta
intersectionCross =
    { id = 15
    , sockets =
        { top = Pink
        , right = Pink
        , bottom = Pink
        , left = Pink
        }
    }



-- lotEntryTUp : TileMeta
-- lotEntryTUp =
-- 23
-- lotEntryTLeft : TileMeta
-- lotEntryTLeft =
--     27
-- lotEntryTRight : TileMeta
-- lotEntryTRight =
--     29
--
