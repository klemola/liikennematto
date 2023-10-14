module Data.TileSet exposing
    ( allTiles
    , defaultTile
    , pairingsForSocket
    )

import Model.TileConfig exposing (Socket(..), Sockets, TileConfig)


pairingsForSocket : Socket -> List Socket
pairingsForSocket socket =
    case socket of
        Red ->
            [ Red, Pink, Yellow, Blue ]

        Green ->
            [ Green ]

        Blue ->
            [ Red, Pink, Yellow ]

        Yellow ->
            [ Red, Yellow, Blue ]

        Pink ->
            [ Red, Yellow, Blue ]


mirroredHorizontally : Sockets -> Sockets
mirroredHorizontally sockets =
    { sockets | left = sockets.right, right = sockets.left }


mirroredVertically : Sockets -> Sockets
mirroredVertically sockets =
    { sockets | top = sockets.bottom, bottom = sockets.top }


rotatedClockwise : Sockets -> Sockets
rotatedClockwise sockets =
    { sockets | top = sockets.left, right = sockets.top, bottom = sockets.right, left = sockets.bottom }


allTiles : List TileConfig
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
    , intersectionTDown
    , intersectionTLeft
    , intersectionTRight
    , intersectionCross
    ]


defaultTile : TileConfig
defaultTile =
    grass


grass : TileConfig
grass =
    { id = 0
    , sockets =
        { top = Green
        , right = Green
        , bottom = Green
        , left = Green
        }
    }


loneRoad : TileConfig
loneRoad =
    { id = 17
    , sockets =
        { top = Green
        , right = Green
        , bottom = Green
        , left = Green
        }
    }


horizontalRoad : TileConfig
horizontalRoad =
    { id = 6
    , sockets =
        { top = Green
        , right = Red
        , bottom = Green
        , left = Red
        }
    }


verticalRoad : TileConfig
verticalRoad =
    { id = 9
    , sockets = rotatedClockwise horizontalRoad.sockets
    }


deadendUp : TileConfig
deadendUp =
    { id = 8
    , sockets =
        { top = Green
        , right = Green
        , bottom = Blue
        , left = Green
        }
    }


deadendRight : TileConfig
deadendRight =
    { id = 2
    , sockets = rotatedClockwise deadendUp.sockets
    }


deadendDown : TileConfig
deadendDown =
    { id = 1
    , sockets = rotatedClockwise deadendRight.sockets
    }


deadendLeft : TileConfig
deadendLeft =
    { id = 4
    , sockets = rotatedClockwise deadendDown.sockets
    }


curveBottomRight : TileConfig
curveBottomRight =
    { id = 3
    , sockets =
        { top = Yellow
        , right = Green
        , bottom = Green
        , left = Yellow
        }
    }


curveBottomLeft : TileConfig
curveBottomLeft =
    { id = 5
    , sockets = rotatedClockwise curveBottomRight.sockets
    }


curveTopLeft : TileConfig
curveTopLeft =
    { id = 12
    , sockets = rotatedClockwise curveBottomLeft.sockets
    }


curveTopRight : TileConfig
curveTopRight =
    { id = 10
    , sockets = rotatedClockwise curveTopLeft.sockets
    }



-- T-intersections cannot be simply rotations of each other, as they have asymmetric sockets (crossings are not on both sides)


intersectionTUp : TileConfig
intersectionTUp =
    { id = 7
    , sockets =
        { top = Pink
        , right = Yellow
        , bottom = Green
        , left = Pink
        }
    }


intersectionTDown : TileConfig
intersectionTDown =
    { id = 14
    , sockets = mirroredVertically intersectionTUp.sockets
    }


intersectionTLeft : TileConfig
intersectionTLeft =
    { id = 11
    , sockets =
        { top = Pink
        , right = Green
        , bottom = Yellow
        , left = Pink
        }
    }


intersectionTRight : TileConfig
intersectionTRight =
    { id = 13
    , sockets = mirroredHorizontally intersectionTLeft.sockets
    }


intersectionCross : TileConfig
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
