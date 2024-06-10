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
    , tileIdByBitmask
    , tileIds
    , tileIdsFromBitmask
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
        , maxGraphPriority
        , mirroredHorizontally
        , mirroredVertically
        , rotatedClockwise
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


tileLookup : Dict TileId TileConfig
tileLookup =
    Dict.fromList
        (List.map
            (\tileConfig -> ( TileConfig.tileConfigId tileConfig, tileConfig ))
            allTilesAndMetaTiles
        )


bitmaskToTileIdLookup : Dict Int TileId
bitmaskToTileIdLookup =
    Dict.fromList
        [ ( 0, TileConfig.tileConfigId loneRoad )
        , ( 1, TileConfig.tileConfigId deadendDown )
        , ( 2, TileConfig.tileConfigId deadendRight )
        , ( 3, TileConfig.tileConfigId curveBottomRight )
        , ( 4, TileConfig.tileConfigId deadendLeft )
        , ( 5, TileConfig.tileConfigId curveBottomLeft )
        , ( 6, TileConfig.tileConfigId horizontalRoad )
        , ( 7, TileConfig.tileConfigId intersectionTUp )
        , ( 8, TileConfig.tileConfigId deadendUp )
        , ( 9, TileConfig.tileConfigId verticalRoad )
        , ( 10, TileConfig.tileConfigId curveTopRight )
        , ( 11, TileConfig.tileConfigId intersectionTLeft )
        , ( 12, TileConfig.tileConfigId curveTopLeft )
        , ( 13, TileConfig.tileConfigId intersectionTRight )
        , ( 14, TileConfig.tileConfigId intersectionTDown )
        , ( 15, TileConfig.tileConfigId intersectionCross )

        -- , ( 23, lotEntryTUp )
        -- , ( 27, lotEntryTLeft )
        -- , ( 29, lotEntryTRight )
        ]


tileById : TileId -> TileConfig
tileById tileId =
    case Dict.get tileId tileLookup of
        Just tileConfig ->
            tileConfig

        Nothing ->
            defaultTile


tileIdByBitmask : Int -> Maybe TileId
tileIdByBitmask bitmask =
    Dict.get bitmask bitmaskToTileIdLookup


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


tileIdsFromBitmask : Int -> List TileId
tileIdsFromBitmask bitmask =
    tileIdByBitmask bitmask
        |> Maybe.withDefault defaultTileId
        |> List.singleton



--
-- Terrain
--


grass : TileConfig
grass =
    TileConfig.Single
        { id = defaultTileId
        , complexity = 0.1
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Nature
        , sockets =
            { top = Green
            , right = Green
            , bottom = Green
            , left = Green
            }
        , baseTileId = Nothing
        }



--
-- Road
--


loneRoad : TileConfig
loneRoad =
    TileConfig.Single
        { id = 17
        , complexity = 0.1
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Green
            , bottom = Green
            , left = Green
            }
        , baseTileId = Nothing
        }


horizontalRoad : TileConfig
horizontalRoad =
    TileConfig.Single
        { id = 6
        , complexity = 0.1
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Red
            , bottom = Green
            , left = Red
            }
        , baseTileId = Nothing
        }


verticalRoad : TileConfig
verticalRoad =
    rotatedClockwise 9 horizontalRoad


deadendUp : TileConfig
deadendUp =
    TileConfig.Single
        { id = 8
        , complexity = 0.1
        , graphPriority = 0
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Green
            , bottom = Blue
            , left = Green
            }
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
        , complexity = 0.2
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Yellow
            , right = Green
            , bottom = Green
            , left = Yellow
            }
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
        , complexity = 0.4
        , graphPriority = 0.2
        , biome = TileConfig.Road
        , sockets =
            { top = Pink
            , right = Yellow
            , bottom = Green
            , left = Pink
            }
        , baseTileId = Nothing
        }


intersectionTDown : TileConfig
intersectionTDown =
    mirroredVertically 14 intersectionTUp


intersectionTLeft : TileConfig
intersectionTLeft =
    TileConfig.Single
        { id = 11
        , complexity = 0.4
        , graphPriority = 0.2
        , biome = TileConfig.Road
        , sockets =
            { top = Pink
            , right = Green
            , bottom = Yellow
            , left = Pink
            }
        , baseTileId = Nothing
        }


intersectionTRight : TileConfig
intersectionTRight =
    mirroredHorizontally 13 intersectionTLeft


intersectionCross : TileConfig
intersectionCross =
    TileConfig.Single
        { id = 15
        , complexity = 0.4
        , graphPriority = 0.2
        , biome = TileConfig.Road
        , sockets =
            { top = Pink
            , right = Pink
            , bottom = Pink
            , left = Pink
            }
        , baseTileId = Nothing
        }


lotEntryTUp : TileConfig
lotEntryTUp =
    TileConfig.Single
        { id = 20
        , complexity = 0.8
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Red
            , bottom = Green
            , left = Red
            }
        , baseTileId = Just 6
        }


lotEntryTRight : TileConfig
lotEntryTRight =
    TileConfig.Single
        { id = 21
        , complexity = 0.8
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Red
            , right = lotEntrySocket
            , bottom = Red
            , left = Green
            }
        , baseTileId = Just 9
        }


lotEntryTLeft : TileConfig
lotEntryTLeft =
    TileConfig.Single
        { id = 22
        , complexity = 0.8
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Red
            , right = Green
            , bottom = Red
            , left = lotEntrySocket
            }
        , baseTileId = Just 9
        }



--
-- Lots
--


lotTopLeftCorner : TileConfig.SingleTile
lotTopLeftCorner =
    { id = 30
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = Green
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    , baseTileId = Nothing
    }


lotTopRightCorner : TileConfig.SingleTile
lotTopRightCorner =
    { id = 31
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = Green
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotBottomRightCorner : TileConfig.SingleTile
lotBottomRightCorner =
    { id = 32
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotBottomLeftCorner : TileConfig.SingleTile
lotBottomLeftCorner =
    { id = 33
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = Green
        }
    , baseTileId = Nothing
    }


lotTopEdge : TileConfig.SingleTile
lotTopEdge =
    { id = 34
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = Green
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotRightEdge : TileConfig.SingleTile
lotRightEdge =
    { id = 35
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotBottomEdge : TileConfig.SingleTile
lotBottomEdge =
    { id = 36
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotLeftEdge : TileConfig.SingleTile
lotLeftEdge =
    { id = 37
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    , baseTileId = Nothing
    }


lotInnerSpace : TileConfig.SingleTile
lotInnerSpace =
    { id = 38
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotDrivewayRight : TileConfig.SingleTile
lotDrivewayRight =
    { id = 40
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = lotDrivewaySocket
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


lotDrivewayLeft : TileConfig.SingleTile
lotDrivewayLeft =
    { id = 41
    , complexity = 0.6
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = lotDrivewaySocket
        }
    , baseTileId = Nothing
    }


lotDrivewayUp : TileConfig.SingleTile
lotDrivewayUp =
    { id = 42
    , complexity = 0.8
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Lot
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = lotDrivewaySocket
        , left = Green
        }
    , baseTileId = Nothing
    }


twoByTwoLot : TileConfig
twoByTwoLot =
    TileConfig.Large
        { id = 100
        , complexity = 0.75
        , biome = TileConfig.Lot
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
        }


threeByThreeLot : TileConfig
threeByThreeLot =
    TileConfig.Large
        { id = 101
        , complexity = 0.8
        , biome = TileConfig.Lot
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
        }


fourByTwoLot : TileConfig
fourByTwoLot =
    TileConfig.Large
        { id = 102
        , complexity = 0.9
        , biome = TileConfig.Lot
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
        }
