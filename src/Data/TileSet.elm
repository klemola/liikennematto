module Data.TileSet exposing
    ( allTiles
    , defaultTile
    , defaultTileId
    , largeTileInnerEdgeSocket
    , pairingsForSocket
    )

import Array
import Model.TileConfig as TileConfig
    exposing
        ( Socket(..)
        , TileConfig
        )


largeTileInnerEdgeSocket : Socket
largeTileInnerEdgeSocket =
    White


lotEntrySocket : Socket
lotEntrySocket =
    Orange


lotDrivewaySocket : Socket
lotDrivewaySocket =
    LightBrown


pairingsForSocket : Socket -> List Socket
pairingsForSocket socket =
    case socket of
        Red ->
            [ Red, Pink, Yellow, Blue ]

        Green ->
            [ Green, White ]

        Blue ->
            [ Red, Pink, Yellow ]

        Yellow ->
            [ Red, Yellow, Blue ]

        Pink ->
            [ Red, Yellow, Blue ]

        Orange ->
            [ LightBrown ]

        Gray ->
            []

        White ->
            [ White, Green ]

        LightBrown ->
            [ Orange ]

        DarkBrown ->
            []


mirroredHorizontally : Int -> TileConfig -> TileConfig
mirroredHorizontally id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | left = sockets.right, right = sockets.left }
                }

        TileConfig.Large _ ->
            tileConfig


mirroredVertically : Int -> TileConfig -> TileConfig
mirroredVertically id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | top = sockets.bottom, bottom = sockets.top }
                }

        TileConfig.Large _ ->
            tileConfig


rotatedClockwise : Int -> TileConfig -> TileConfig
rotatedClockwise id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | top = sockets.left, right = sockets.top, bottom = sockets.right, left = sockets.bottom }
                }

        TileConfig.Large _ ->
            tileConfig


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

    --
    -- , lotEntryTUp (not yet used by any lots)
    , lotEntryTLeft
    , lotEntryTRight

    --
    , twoByTwoLot
    , threeByThreeLot
    ]


defaultTile : TileConfig
defaultTile =
    grass


defaultTileId =
    0



--
-- Terrain
--


grass : TileConfig
grass =
    TileConfig.Single
        { id = defaultTileId
        , sockets =
            { top = Green
            , right = Green
            , bottom = Green
            , left = Green
            }
        }



--
-- Road
--


loneRoad : TileConfig
loneRoad =
    TileConfig.Single
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
    TileConfig.Single
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
    rotatedClockwise 9 horizontalRoad


deadendUp : TileConfig
deadendUp =
    TileConfig.Single
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
    rotatedClockwise 2 deadendUp


deadendDown : TileConfig
deadendDown =
    rotatedClockwise 1 deadendRight


deadendLeft : TileConfig
deadendLeft =
    rotatedClockwise 4 deadendDown


curveBottomRight : TileConfig
curveBottomRight =
    TileConfig.Single
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
    rotatedClockwise 5 curveBottomRight


curveTopLeft : TileConfig
curveTopLeft =
    rotatedClockwise 12 curveBottomLeft


curveTopRight : TileConfig
curveTopRight =
    rotatedClockwise 10 curveTopLeft



-- T-intersections cannot be simply rotations of each other, as they have asymmetric sockets (crossings are not on both sides)


intersectionTUp : TileConfig
intersectionTUp =
    TileConfig.Single
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
    mirroredVertically 14 intersectionTUp


intersectionTLeft : TileConfig
intersectionTLeft =
    TileConfig.Single
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
    mirroredHorizontally 13 intersectionTLeft


intersectionCross : TileConfig
intersectionCross =
    TileConfig.Single
        { id = 15
        , sockets =
            { top = Pink
            , right = Pink
            , bottom = Pink
            , left = Pink
            }
        }


lotEntryTUp : TileConfig
lotEntryTUp =
    TileConfig.Single
        { id = 23
        , sockets =
            { top = lotEntrySocket
            , right = Red
            , bottom = Green
            , left = Red
            }
        }


lotEntryTLeft : TileConfig
lotEntryTLeft =
    TileConfig.Single
        { id = 27
        , sockets =
            { top = Red
            , right = Green
            , bottom = Red
            , left = lotEntrySocket
            }
        }


lotEntryTRight : TileConfig
lotEntryTRight =
    TileConfig.Single
        { id = 29
        , sockets =
            { top = Red
            , right = lotEntrySocket
            , bottom = Red
            , left = Green
            }
        }



--
-- Lots
--


lotTopLeftCorner : TileConfig.SingleTile
lotTopLeftCorner =
    { id = 30
    , sockets =
        { top = Green
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    }


lotTopRightCorner : TileConfig.SingleTile
lotTopRightCorner =
    { id = 31
    , sockets =
        { top = Green
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    }


lotBottomRightCorner : TileConfig.SingleTile
lotBottomRightCorner =
    { id = 32
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    }


lotBottomLeftCorner : TileConfig.SingleTile
lotBottomLeftCorner =
    { id = 33
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = Green
        }
    }


lotInnerSpace : TileConfig.SingleTile
lotInnerSpace =
    { id = 34
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    }


lotDrivewayRight : TileConfig.SingleTile
lotDrivewayRight =
    { id = 35
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = lotDrivewaySocket
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    }


lotDrivewayLeft : TileConfig.SingleTile
lotDrivewayLeft =
    { id = 36
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = lotDrivewaySocket
        }
    }


lotDrivewayUp : TileConfig.SingleTile
lotDrivewayUp =
    { id = 37
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = lotDrivewaySocket
        , left = Green
        }
    }


twoByTwoLot : TileConfig
twoByTwoLot =
    TileConfig.Large
        { id = 100
        , tiles =
            Array.fromList
                [ lotTopLeftCorner, lotTopRightCorner, lotBottomLeftCorner, lotDrivewayRight ]
        , width = 2
        , height = 2
        , anchorIndex = 3
        }


threeByThreeLot : TileConfig
threeByThreeLot =
    TileConfig.Large
        { id = 101
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotInnerSpace
                , lotTopRightCorner

                --
                , lotInnerSpace
                , lotInnerSpace
                , lotInnerSpace

                --
                , lotDrivewayLeft
                , lotInnerSpace
                , lotBottomRightCorner
                ]
        , width = 3
        , height = 3
        , anchorIndex = 6
        }
