module Data.TileSet exposing
    ( allTiles
    , bottomEdgeTileIds
    , bottomLeftCornerTileIds
    , bottomRightCornerTileIds
    , isTileLotEntryTile
    , leftEdgeTileIds
    , pairingsForSocket
    , rightEdgeTileIds
    , roadConnectionDirectionsByTile
    , tileById
    , tileIds
    , topEdgeTileIds
    , topLeftCornerTileIds
    , topRightCornerTileIds
    )

import Array
import Dict exposing (Dict)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import List.Nonempty
import Tilemap.TileConfig as TileConfig
    exposing
        ( Socket(..)
        , TileConfig
        , TileId
        , socketByDirection
        )


defaultSocket : Socket
defaultSocket =
    Green


largeTileInnerEdgeSocket : Socket
largeTileInnerEdgeSocket =
    White


lotEntrySocket : Socket
lotEntrySocket =
    Orange


lotDrivewaySocket : Socket
lotDrivewaySocket =
    LightBrown


roadConnectionSockets : List Socket
roadConnectionSockets =
    [ Red, Pink, Yellow, Blue, lotEntrySocket ]


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

        Orange ->
            [ LightBrown ]

        Gray ->
            []

        White ->
            [ White ]

        LightBrown ->
            [ Orange ]

        DarkBrown ->
            []


defaultTile : TileConfig
defaultTile =
    grass


defaultTileId : TileId
defaultTileId =
    0


mirroredHorizontally : TileId -> TileConfig -> TileConfig
mirroredHorizontally id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets, complexity, baseTileId } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | left = sockets.right, right = sockets.left }
                , complexity = complexity
                , baseTileId = baseTileId
                }

        TileConfig.Large _ ->
            tileConfig


mirroredVertically : TileId -> TileConfig -> TileConfig
mirroredVertically id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets, complexity, baseTileId } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | top = sockets.bottom, bottom = sockets.top }
                , complexity = complexity
                , baseTileId = baseTileId
                }

        TileConfig.Large _ ->
            tileConfig


rotatedClockwise : TileId -> TileConfig -> TileConfig
rotatedClockwise id tileConfig =
    case tileConfig of
        TileConfig.Single { sockets, complexity, baseTileId } ->
            TileConfig.Single
                { id = id
                , sockets = { sockets | top = sockets.left, right = sockets.top, bottom = sockets.right, left = sockets.bottom }
                , complexity = complexity
                , baseTileId = baseTileId
                }

        TileConfig.Large _ ->
            tileConfig


allTiles : List TileConfig
allTiles =
    [ grass

    --
    , loneRoad
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
    , lotEntryTUp
    , lotEntryTLeft
    , lotEntryTRight

    --
    , twoByTwoLot
    , threeByThreeLot
    , fourByTwoLot
    ]


allTilesAndMetaTiles : List TileConfig
allTilesAndMetaTiles =
    allTiles
        ++ [ TileConfig.Single lotTopLeftCorner
           , TileConfig.Single lotTopRightCorner
           , TileConfig.Single lotBottomLeftCorner
           , TileConfig.Single lotBottomRightCorner
           , TileConfig.Single lotTopEdge
           , TileConfig.Single lotRightEdge
           , TileConfig.Single lotBottomEdge
           , TileConfig.Single lotLeftEdge
           , TileConfig.Single lotInnerSpace
           , TileConfig.Single lotDrivewayLeft
           , TileConfig.Single lotDrivewayRight
           , TileConfig.Single lotDrivewayUp
           ]


tiles : Dict TileId TileConfig
tiles =
    Dict.fromList
        (List.map
            (\tileConfig -> ( TileConfig.tileConfigId tileConfig, tileConfig ))
            allTilesAndMetaTiles
        )


tileById : TileId -> TileConfig
tileById tileId =
    case Dict.get tileId tiles of
        Just tileConfig ->
            tileConfig

        Nothing ->
            defaultTile


isTileLotEntryTile : TileId -> Bool
isTileLotEntryTile =
    tileById
        >> TileConfig.socketsList
        >> List.Nonempty.any ((==) lotEntrySocket)


roadConnectionDirectionsByTile : TileConfig -> List OrthogonalDirection
roadConnectionDirectionsByTile tileConfig =
    let
        sockets_ =
            TileConfig.sockets tileConfig
    in
    OrthogonalDirection.all
        |> List.filter
            (\dir ->
                -- TODO: optimize, using List instead of Set due to non-comparable Socket
                List.member (socketByDirection sockets_ dir) roadConnectionSockets
            )


tileIds : List TileId
tileIds =
    List.map TileConfig.tileConfigId allTiles


topLeftCornerTileIds : List TileId
topLeftCornerTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.top == defaultSocket && sockets.left == defaultSocket)
        )
        allTiles


topRightCornerTileIds : List TileId
topRightCornerTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.top == defaultSocket && sockets.right == defaultSocket)
        )
        allTiles


bottomLeftCornerTileIds : List TileId
bottomLeftCornerTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.bottom == defaultSocket && sockets.left == defaultSocket)
        )
        allTiles


bottomRightCornerTileIds : List TileId
bottomRightCornerTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.bottom == defaultSocket && sockets.right == defaultSocket)
        )
        allTiles


leftEdgeTileIds : List TileId
leftEdgeTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.left == defaultSocket)
        )
        allTiles


rightEdgeTileIds : List TileId
rightEdgeTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.right == defaultSocket)
        )
        allTiles


topEdgeTileIds : List TileId
topEdgeTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.top == defaultSocket)
        )
        allTiles


bottomEdgeTileIds : List TileId
bottomEdgeTileIds =
    List.filterMap
        (compatibleTileId
            (\sockets -> sockets.bottom == defaultSocket)
        )
        allTiles


compatibleTileId : (TileConfig.Sockets -> Bool) -> TileConfig -> Maybe TileId
compatibleTileId matcher tc =
    if matcher (TileConfig.sockets tc) then
        Just (TileConfig.tileConfigId tc)

    else
        Nothing



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
        , complexity = 0.1
        , baseTileId = Nothing
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
        , complexity = 0.1
        , baseTileId = Nothing
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
        , complexity = 0.1
        , baseTileId = Nothing
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
        , complexity = 0.1
        , baseTileId = Nothing
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
        , complexity = 0.2
        , baseTileId = Nothing
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
        , complexity = 0.4
        , baseTileId = Nothing
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
        , complexity = 0.4
        , baseTileId = Nothing
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
        , complexity = 0.4
        , baseTileId = Nothing
        }


lotEntryTUp : TileConfig
lotEntryTUp =
    TileConfig.Single
        { id = 20
        , sockets =
            { top = lotEntrySocket
            , right = Red
            , bottom = Green
            , left = Red
            }
        , complexity = 0.8
        , baseTileId = Just 6
        }


lotEntryTRight : TileConfig
lotEntryTRight =
    TileConfig.Single
        { id = 21
        , sockets =
            { top = Red
            , right = lotEntrySocket
            , bottom = Red
            , left = Green
            }
        , complexity = 0.8
        , baseTileId = Just 9
        }


lotEntryTLeft : TileConfig
lotEntryTLeft =
    TileConfig.Single
        { id = 22
        , sockets =
            { top = Red
            , right = Green
            , bottom = Red
            , left = lotEntrySocket
            }
        , complexity = 0.8
        , baseTileId = Just 9
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
    , complexity = 0.6
    , baseTileId = Nothing
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
    , complexity = 0.6
    , baseTileId = Nothing
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
    , complexity = 0.6
    , baseTileId = Nothing
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
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotTopEdge : TileConfig.SingleTile
lotTopEdge =
    { id = 34
    , sockets =
        { top = Green
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotRightEdge : TileConfig.SingleTile
lotRightEdge =
    { id = 35
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotBottomEdge : TileConfig.SingleTile
lotBottomEdge =
    { id = 36
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotLeftEdge : TileConfig.SingleTile
lotLeftEdge =
    { id = 37
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotInnerSpace : TileConfig.SingleTile
lotInnerSpace =
    { id = 38
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotDrivewayRight : TileConfig.SingleTile
lotDrivewayRight =
    { id = 40
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = lotDrivewaySocket
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotDrivewayLeft : TileConfig.SingleTile
lotDrivewayLeft =
    { id = 41
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = lotDrivewaySocket
        }
    , complexity = 0.6
    , baseTileId = Nothing
    }


lotDrivewayUp : TileConfig.SingleTile
lotDrivewayUp =
    { id = 42
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = lotDrivewaySocket
        , left = Green
        }
    , complexity = 0.8
    , baseTileId = Nothing
    }


twoByTwoLot : TileConfig
twoByTwoLot =
    TileConfig.Large
        { id = 100
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopRightCorner

                --
                , lotBottomLeftCorner
                , lotDrivewayRight
                ]
        , width = 2
        , height = 2
        , anchorIndex = 3
        , complexity = 0.75
        }


threeByThreeLot : TileConfig
threeByThreeLot =
    TileConfig.Large
        { id = 101
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopEdge
                , lotTopRightCorner

                --
                , lotLeftEdge
                , lotInnerSpace
                , lotRightEdge

                --
                , lotDrivewayLeft
                , lotBottomEdge
                , lotBottomRightCorner
                ]
        , width = 3
        , height = 3
        , anchorIndex = 6
        , complexity = 0.8
        }


fourByTwoLot : TileConfig
fourByTwoLot =
    TileConfig.Large
        { id = 102
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopEdge
                , lotTopEdge
                , lotTopRightCorner

                --
                , lotDrivewayUp
                , lotBottomEdge
                , lotBottomEdge
                , lotBottomRightCorner
                ]
        , width = 4
        , height = 2
        , anchorIndex = 4
        , complexity = 0.9
        }
