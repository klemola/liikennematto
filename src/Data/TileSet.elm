module Data.TileSet exposing
    ( allTiles
    , allTilesAmount
    , basicRoadTiles
    , decorativeTiles
    , defaultSocket
    , defaultTiles
    , extractLotEntryTile
    , lotDrivewaySocket
    , lotDrivewayTileIds
    , lotEntrySocket
    , lotTiles
    , pairingsForSocket
    , roadConnectionDirectionsByTile
    , threeByThreeLotLargeTile
    , threeByTwoLotALargeTile
    , tileById
    , tileIdByBitmask
    , tileIdsByOrthogonalMatch
    , tileIdsFromBitmask
    , tilesByBaseTileId
    , twoByTwoLotLargeTile
    )

import Array
import Dict exposing (Dict)
import Lib.Bitmask exposing (OrthogonalMatch)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import List.Nonempty
import Set exposing (Set)
import Tilemap.TileConfig as TileConfig
    exposing
        ( LargeTile
        , Socket(..)
        , TileConfig
        , TileId
        , maxGraphPriority
        , mirroredHorizontally
        , mirroredVertically
        , rotatedClockwise
        , socketByDirection
        )



--
-- Sockets definition
--


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



-- Tile sets and lookup


defaultTile : TileConfig
defaultTile =
    grass


defaultTileId : TileId
defaultTileId =
    0


loneRoadTileId : TileId
loneRoadTileId =
    17


basicRoadTiles : Set TileId
basicRoadTiles =
    Set.fromList [ TileConfig.tileConfigId horizontalRoad, TileConfig.tileConfigId verticalRoad ]


defaultWeight : Float
defaultWeight =
    0.5


allTiles : List TileConfig
allTiles =
    [ grass
    , singleNature
    , twoByTwoNature

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
    , twoByTwoLot
    , threeByThreeLot
    , threeByTwoLotA
    , threeByTwoLotB
    , twoByThreeLot
    ]


allTilesAmount : Int
allTilesAmount =
    List.length allTiles


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
           , TileConfig.Single natureTopLeftCorner
           , TileConfig.Single natureTopRightCorner
           , TileConfig.Single natureBottomLeftCorner
           , TileConfig.Single natureBottomRightCorner
           ]


tileLookup : Dict TileId TileConfig
tileLookup =
    Dict.fromList
        (List.map
            (\tileConfig -> ( TileConfig.tileConfigId tileConfig, tileConfig ))
            allTilesAndMetaTiles
        )


tileById : TileId -> TileConfig
tileById tileId =
    case Dict.get tileId tileLookup of
        Just tileConfig ->
            tileConfig

        Nothing ->
            defaultTile


lotTiles : List TileConfig
lotTiles =
    List.filter (\tileConfig -> TileConfig.biome tileConfig == TileConfig.Lot) allTiles


decorativeTiles : List TileConfig
decorativeTiles =
    List.filter
        (\tileConfig ->
            TileConfig.biome tileConfig == TileConfig.Nature
        )
        allTiles


defaultTiles : List TileConfig
defaultTiles =
    List.filter (\tileConfig -> TileConfig.tileConfigId tileConfig /= loneRoadTileId) allTiles


baseTileLookup : Dict TileId (List TileConfig)
baseTileLookup =
    List.foldl
        (\tileConfig groups ->
            case TileConfig.baseTileId tileConfig of
                Just baseTileId ->
                    let
                        currentTiles =
                            Dict.get baseTileId groups
                                |> Maybe.withDefault []
                    in
                    groups |> Dict.insert baseTileId (tileConfig :: currentTiles)

                Nothing ->
                    groups
        )
        Dict.empty
        allTiles


tilesByBaseTileId : TileId -> List TileConfig
tilesByBaseTileId baseTileId =
    Dict.get baseTileId baseTileLookup
        |> Maybe.withDefault []



-- Bitmask based lookup


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
        ]


tileIdByBitmask : Int -> Maybe TileId
tileIdByBitmask bitmask =
    Dict.get bitmask bitmaskToTileIdLookup


tileIdsFromBitmask : Int -> List TileId
tileIdsFromBitmask bitmask =
    tileIdByBitmask bitmask
        |> Maybe.withDefault defaultTileId
        |> List.singleton



-- Orthogonal neighbor based lookup


noOrthogonalMatch : OrthogonalMatch
noOrthogonalMatch =
    { up = False
    , left = False
    , right = False
    , down = False
    }


tileIdsByOrthogonalMatch : List TileConfig -> OrthogonalMatch -> List TileId
tileIdsByOrthogonalMatch tileSet ({ up, left, right, down } as neighbors) =
    if neighbors == noOrthogonalMatch then
        List.map TileConfig.tileConfigId tileSet

    else
        let
            conditions sockets =
                [ ( up, sockets.top ), ( left, sockets.left ), ( right, sockets.right ), ( down, sockets.bottom ) ]
                    |> List.all
                        (\( hasNeighbor, socket ) ->
                            if hasNeighbor then
                                socket == defaultSocket

                            else
                                True
                        )
        in
        List.filterMap (compatibleTileId conditions) tileSet


compatibleTileId : (TileConfig.Sockets -> Bool) -> TileConfig -> Maybe TileId
compatibleTileId matcher tc =
    if matcher (TileConfig.sockets tc) then
        Just (TileConfig.tileConfigId tc)

    else
        Nothing



--
-- Queries
--


extractLotEntryTile : TileId -> Maybe ( TileConfig, OrthogonalDirection )
extractLotEntryTile tileId =
    let
        tileConfig =
            tileById tileId
    in
    TileConfig.socketsList tileConfig
        |> List.Nonempty.toList
        |> findLotEntryDirection
        |> Maybe.map (Tuple.pair tileConfig)


findLotEntryDirection : List ( OrthogonalDirection, Socket ) -> Maybe OrthogonalDirection
findLotEntryDirection sockets =
    case sockets of
        [] ->
            Nothing

        x :: xs ->
            let
                ( dir, socket ) =
                    x
            in
            if socket == lotEntrySocket then
                Just dir

            else
                findLotEntryDirection xs


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



--
-- Terrain tiles
--


grass : TileConfig
grass =
    TileConfig.Single
        { id = defaultTileId

        -- Make the "default/filler" not very likely to appear, as it is a last resort
        , weight = 0.1
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
-- Road tiles
--


loneRoad : TileConfig
loneRoad =
    TileConfig.Single
        { id = loneRoadTileId
        , weight = defaultWeight
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
        , weight = 0.5
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
        , weight = 0.5
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
        , weight = 0.5
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
        , weight = 0.3
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
        , weight = 0.3
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
        , weight = 0.3
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
        , weight = 0.8
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
        , weight = 0.8
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
        , weight = 0.8
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
-- Lot tiles
--


lotDrivewayTileIds : List TileId
lotDrivewayTileIds =
    [ lotDrivewayLeft, lotDrivewayRight, lotDrivewayUp ]
        |> List.map .id


lotTopLeftCorner : TileConfig.SingleTile
lotTopLeftCorner =
    { id = 30
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    , weight = defaultWeight
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
    TileConfig.Large twoByTwoLotLargeTile


twoByTwoLotLargeTile : LargeTile
twoByTwoLotLargeTile =
    { id = 100
    , weight = 0.9
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
    TileConfig.Large threeByThreeLotLargeTile


threeByThreeLotLargeTile : LargeTile
threeByThreeLotLargeTile =
    { id = 101
    , weight = 0.9
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


threeByTwoLotA : TileConfig
threeByTwoLotA =
    TileConfig.Large threeByTwoLotALargeTile


threeByTwoLotALargeTile : LargeTile
threeByTwoLotALargeTile =
    { id = 102
    , weight = 0.9
    , biome = TileConfig.Lot
    , tiles =
        Array.fromList
            [ lotTopLeftCorner
            , lotTopEdge
            , lotTopRightCorner

            --
            , lotDrivewayUp
            , lotBottomEdge
            , lotBottomRightCorner
            ]
    , width = 3
    , height = 2
    , anchorIndex = 3
    }


threeByTwoLotB : TileConfig
threeByTwoLotB =
    TileConfig.Large
        { id = 103
        , weight = 0.9
        , biome = TileConfig.Lot
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopEdge
                , lotTopRightCorner

                --
                , lotDrivewayRight
                , lotBottomEdge
                , lotBottomRightCorner
                ]
        , width = 3
        , height = 2
        , anchorIndex = 3
        }


twoByThreeLot : TileConfig
twoByThreeLot =
    TileConfig.Large
        { id = 104
        , weight = 0.9
        , biome = TileConfig.Lot
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopRightCorner

                --
                , lotLeftEdge
                , lotRightEdge

                --
                , lotDrivewayUp
                , lotBottomRightCorner
                ]
        , width = 2
        , height = 3
        , anchorIndex = 4
        }



--
-- Nature tiles
--


natureTopLeftCorner : TileConfig.SingleTile
natureTopLeftCorner =
    { id = 50
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = Green
        , right = largeTileInnerEdgeSocket
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    , baseTileId = Nothing
    }


natureTopRightCorner : TileConfig.SingleTile
natureTopRightCorner =
    { id = 51
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = Green
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


natureBottomRightCorner : TileConfig.SingleTile
natureBottomRightCorner =
    { id = 52
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = Green
        , left = largeTileInnerEdgeSocket
        }
    , baseTileId = Nothing
    }


natureBottomLeftCorner : TileConfig.SingleTile
natureBottomLeftCorner =
    { id = 53
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = largeTileInnerEdgeSocket
        , bottom = Green
        , left = Green
        }
    , baseTileId = Nothing
    }


singleNature : TileConfig
singleNature =
    TileConfig.Single
        { id = 200
        , weight = 0.1
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


twoByTwoNature : TileConfig
twoByTwoNature =
    TileConfig.Large
        { id = 201
        , weight = 0.1
        , biome = TileConfig.Nature
        , tiles =
            Array.fromList
                [ natureTopLeftCorner
                , natureTopRightCorner

                --
                , natureBottomLeftCorner
                , natureBottomRightCorner
                ]
        , width = 2
        , height = 2
        , anchorIndex = 0
        }
