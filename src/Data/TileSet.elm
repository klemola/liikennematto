module Data.TileSet exposing
    ( ConnectionsByTile
    , allTiles
    , allTilesAmount
    , basicRoadTiles
    , connectionsByTile
    , decorativeTiles
    , defaultSocket
    , defaultTileId
    , defaultTiles
    , extractLotEntryTile
    , horizontalRoadLotEntryUp
    , lotDrivewaySocket
    , lotDrivewayTileIds
    , lotEntrySocket
    , lotTiles
    , pairingsForSocket
    , threeByThreeLotLeftLargeTile
    , threeByTwoLotUpLargeTile
    , tileById
    , tileIdByBitmask
    , tileIdsByOrthogonalMatch
    , tilesByBaseTileId
    , twoByTwoLotRightLargeTile
    , verticalRoadLotEntryLeft
    , verticalRoadLotEntryRight
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
    [ Red, Pink, Yellow, Blue ]


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
    natureSingle1


defaultTileId : TileId
defaultTileId =
    TileConfig.tileConfigId natureSingle1


loneRoadTileId : TileId
loneRoadTileId =
    TileConfig.tileConfigId loneRoad


basicRoadTiles : Set TileId
basicRoadTiles =
    Set.fromList [ TileConfig.tileConfigId horizontalRoad, TileConfig.tileConfigId verticalRoad ]


defaultWeight : Float
defaultWeight =
    0.5


allTiles : List TileConfig
allTiles =
    [ natureSingle1
    , natureSingle2
    , natureSingle3
    , natureSingle4
    , natureDouble1
    , natureDouble2
    , natureQuad1
    , natureQuad2

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
    , horizontalRoadLotEntryUp
    , verticalRoadLotEntryLeft
    , verticalRoadLotEntryRight
    , deadendLeftLotEntryUp
    , deadendLeftLotEntryLeft
    , deadendRightLotEntryUp
    , deadendRightLotEntryRight
    , deadendDownLotEntryLeft
    , deadendDownLotEntryRight
    , deadendUpLotEntryLeft
    , deadendUpLotEntryRight
    , deadendUpLotEntryUp
    , intersectionTDownLotEntryUp
    , intersectionTLeftLotEntryRight
    , intersectionTRightLotEntryLeft
    , curveBottomRightLotEntryRight
    , curveBottomLeftLotEntryLeft
    , curveTopLeftLotEntryLeft
    , curveTopLeftLotEntryUp
    , curveTopRightLotEntryRight
    , curveTopRightLotEntryUp
    , twoByTwoLotRight
    , threeByThreeLotLeft
    , threeByTwoLotUp
    , threeByTwoLotLeft
    , twoByThreeLotUp
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
           , TileConfig.Single natureEndUp
           , TileConfig.Single natureEndDown
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


type alias ConnectionsByTile =
    { roadConnections : List OrthogonalDirection
    , lotConnection : Maybe OrthogonalDirection
    }


connectionsByTile : TileConfig -> ConnectionsByTile
connectionsByTile tileConfig =
    List.Nonempty.foldl
        (\( dir, socket ) acc ->
            if List.member socket roadConnectionSockets then
                { acc | roadConnections = dir :: acc.roadConnections }

            else if socket == lotEntrySocket then
                { acc | lotConnection = Just dir }

            else
                acc
        )
        { roadConnections = [], lotConnection = Nothing }
        (TileConfig.socketsList tileConfig)



--
-- Road tiles
--


loneRoad : TileConfig
loneRoad =
    TileConfig.Single
        { id = 17
        , name = "RoadLone"
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
        , name = "RoadHorizontal"
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
    rotatedClockwise 9 "RoadVertical" horizontalRoad


deadendUp : TileConfig
deadendUp =
    TileConfig.Single
        { id = 8
        , name = "RoadDeadendUp"
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
    rotatedClockwise 2 "RoadDeadendRight" deadendUp


deadendDown : TileConfig
deadendDown =
    rotatedClockwise 1 "RoadDeadendDown" deadendRight


deadendLeft : TileConfig
deadendLeft =
    rotatedClockwise 4 "RoadDeadendLeft" deadendDown


curveBottomRight : TileConfig
curveBottomRight =
    TileConfig.Single
        { id = 3
        , name = "RoadCurveBottomRight"
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
    rotatedClockwise 5 "RoadCurveBottomLeft" curveBottomRight


curveTopLeft : TileConfig
curveTopLeft =
    rotatedClockwise 12 "RoadCurveTopLeft" curveBottomLeft


curveTopRight : TileConfig
curveTopRight =
    rotatedClockwise 10 "RoadCurveTopRight" curveTopLeft



-- T-intersections cannot be simply rotations of each other, as they have asymmetric sockets (crossings are not on both sides)


intersectionTUp : TileConfig
intersectionTUp =
    TileConfig.Single
        { id = 7
        , name = "RoadIntersectionTUp"
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
    mirroredVertically 14 "RoadIntersectionTDown" intersectionTUp


intersectionTLeft : TileConfig
intersectionTLeft =
    TileConfig.Single
        { id = 11
        , name = "RoadIntersectionTLeft"
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
    mirroredHorizontally 13 "RoadIntersectionTRight" intersectionTLeft


intersectionCross : TileConfig
intersectionCross =
    TileConfig.Single
        { id = 15
        , name = "RoadIntersectionCrossroads"
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


horizontalRoadLotEntryUp : TileConfig
horizontalRoadLotEntryUp =
    TileConfig.Single
        { id = 50
        , name = "RoadHorizontalLotEntryUp"
        , weight = 1.0
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


verticalRoadLotEntryRight : TileConfig
verticalRoadLotEntryRight =
    TileConfig.Single
        { id = 51
        , name = "RoadVerticalLotEntryRight"
        , weight = 1.0
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


verticalRoadLotEntryLeft : TileConfig
verticalRoadLotEntryLeft =
    TileConfig.Single
        { id = 52
        , name = "RoadVerticalLotEntryLeft"
        , weight = 1.0
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


deadendLeftLotEntryUp : TileConfig
deadendLeftLotEntryUp =
    TileConfig.Single
        { id = 53
        , name = "RoadDeadendLeftLotEntryUp"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Blue
            , bottom = Green
            , left = Green
            }
        , baseTileId = Just 4
        }


deadendLeftLotEntryLeft : TileConfig
deadendLeftLotEntryLeft =
    TileConfig.Single
        { id = 54
        , name = "RoadDeadendLeftLotEntryLeft"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Blue
            , bottom = Green
            , left = lotEntrySocket
            }
        , baseTileId = Just 4
        }


deadendRightLotEntryUp : TileConfig
deadendRightLotEntryUp =
    TileConfig.Single
        { id = 55
        , name = "RoadDeadendRightLotEntryUp"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Green
            , bottom = Green
            , left = Blue
            }
        , baseTileId = Just 2
        }


deadendRightLotEntryRight : TileConfig
deadendRightLotEntryRight =
    TileConfig.Single
        { id = 56
        , name = "RoadDeadendRightLotEntryRight"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = lotEntrySocket
            , bottom = Green
            , left = Blue
            }
        , baseTileId = Just 2
        }


deadendDownLotEntryLeft : TileConfig
deadendDownLotEntryLeft =
    TileConfig.Single
        { id = 57
        , name = "RoadDeadendDownLotEntryLeft"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Blue
            , right = Green
            , bottom = Green
            , left = lotEntrySocket
            }
        , baseTileId = Just 1
        }


deadendDownLotEntryRight : TileConfig
deadendDownLotEntryRight =
    TileConfig.Single
        { id = 58
        , name = "RoadDeadendDownLotEntryRight"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Blue
            , right = lotEntrySocket
            , bottom = Green
            , left = Green
            }
        , baseTileId = Just 1
        }


deadendUpLotEntryLeft : TileConfig
deadendUpLotEntryLeft =
    TileConfig.Single
        { id = 59
        , name = "RoadDeadendUpLotEntryLeft"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Green
            , bottom = Blue
            , left = lotEntrySocket
            }
        , baseTileId = Just 8
        }


deadendUpLotEntryRight : TileConfig
deadendUpLotEntryRight =
    TileConfig.Single
        { id = 60
        , name = "RoadDeadendUpLotEntryRight"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = lotEntrySocket
            , bottom = Blue
            , left = Green
            }
        , baseTileId = Just 8
        }


deadendUpLotEntryUp : TileConfig
deadendUpLotEntryUp =
    TileConfig.Single
        { id = 61
        , name = "RoadDeadendUpLotEntryUp"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Green
            , bottom = Blue
            , left = Green
            }
        , baseTileId = Just 8
        }


intersectionTDownLotEntryUp : TileConfig
intersectionTDownLotEntryUp =
    TileConfig.Single
        { id = 62
        , name = "RoadIntersectionTDownLotEntryUp"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Yellow
            , bottom = Pink
            , left = Pink
            }
        , baseTileId = Just 14
        }


intersectionTRightLotEntryLeft : TileConfig
intersectionTRightLotEntryLeft =
    TileConfig.Single
        { id = 63
        , name = "RoadIntersectionTRightLotEntryLeft"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Pink
            , right = Pink
            , bottom = Yellow
            , left = lotEntrySocket
            }
        , baseTileId = Just 13
        }


intersectionTLeftLotEntryRight : TileConfig
intersectionTLeftLotEntryRight =
    TileConfig.Single
        { id = 64
        , name = "RoadIntersectionTLeftLotEntryRight"
        , weight = 1.0
        , graphPriority = 0.1
        , biome = TileConfig.Road
        , sockets =
            { top = Pink
            , right = lotEntrySocket
            , bottom = Yellow
            , left = Pink
            }
        , baseTileId = Just 11
        }


curveBottomRightLotEntryRight : TileConfig
curveBottomRightLotEntryRight =
    TileConfig.Single
        { id = 65
        , name = "RoadCurveBottomRightLotEntryRight"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Yellow
            , right = lotEntrySocket
            , bottom = Green
            , left = Yellow
            }
        , baseTileId = Just 3
        }


curveBottomLeftLotEntryLeft : TileConfig
curveBottomLeftLotEntryLeft =
    TileConfig.Single
        { id = 66
        , name = "RoadCurveBottomLeftLotEntryLeft"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Yellow
            , right = Yellow
            , bottom = Green
            , left = lotEntrySocket
            }
        , baseTileId = Just 5
        }


curveTopLeftLotEntryLeft : TileConfig
curveTopLeftLotEntryLeft =
    TileConfig.Single
        { id = 67
        , name = "RoadCurveTopLeftLotEntryLeft"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = Yellow
            , bottom = Yellow
            , left = lotEntrySocket
            }
        , baseTileId = Just 12
        }


curveTopLeftLotEntryUp : TileConfig
curveTopLeftLotEntryUp =
    TileConfig.Single
        { id = 68
        , name = "RoadCurveTopLeftLotEntryUp"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Yellow
            , bottom = Yellow
            , left = Green
            }
        , baseTileId = Just 12
        }


curveTopRightLotEntryRight : TileConfig
curveTopRightLotEntryRight =
    TileConfig.Single
        { id = 69
        , name = "RoadCurveTopRightLotEntryRight"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = Green
            , right = lotEntrySocket
            , bottom = Yellow
            , left = Yellow
            }
        , baseTileId = Just 10
        }


curveTopRightLotEntryUp : TileConfig
curveTopRightLotEntryUp =
    TileConfig.Single
        { id = 70
        , name = "RoadCurveTopRightLotEntryUp"
        , weight = 0.5
        , graphPriority = maxGraphPriority
        , biome = TileConfig.Road
        , sockets =
            { top = lotEntrySocket
            , right = Green
            , bottom = Yellow
            , left = Yellow
            }
        , baseTileId = Just 10
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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
    , name = "_subgrid"
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


twoByTwoLotRight : TileConfig
twoByTwoLotRight =
    TileConfig.Large twoByTwoLotRightLargeTile


twoByTwoLotRightLargeTile : LargeTile
twoByTwoLotRightLargeTile =
    { id = 100
    , name = "TwoByTwoLotRight"
    , weight = 1.0
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
    , entryDirection = Just OrthogonalDirection.Left
    }


threeByThreeLotLeft : TileConfig
threeByThreeLotLeft =
    TileConfig.Large threeByThreeLotLeftLargeTile


threeByThreeLotLeftLargeTile : LargeTile
threeByThreeLotLeftLargeTile =
    { id = 101
    , name = "ThreeByThreeLotLeft"
    , weight = 1.0
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
    , entryDirection = Just OrthogonalDirection.Right
    }


threeByTwoLotUp : TileConfig
threeByTwoLotUp =
    TileConfig.Large threeByTwoLotUpLargeTile


threeByTwoLotUpLargeTile : LargeTile
threeByTwoLotUpLargeTile =
    { id = 102
    , name = "ThreeByTwoLotUp"
    , weight = 1.0
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
    , entryDirection = Just OrthogonalDirection.Up
    }


threeByTwoLotLeft : TileConfig
threeByTwoLotLeft =
    TileConfig.Large
        { id = 103
        , name = "ThreeByTwoLotLeft"
        , weight = 1.0
        , biome = TileConfig.Lot
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopEdge
                , lotTopRightCorner

                --
                , lotDrivewayLeft
                , lotBottomEdge
                , lotBottomRightCorner
                ]
        , width = 3
        , height = 2
        , anchorIndex = 3
        , entryDirection = Just OrthogonalDirection.Right
        }


twoByThreeLotUp : TileConfig
twoByThreeLotUp =
    TileConfig.Large
        { id = 104
        , name = "TwoByThreeLotUp"
        , weight = 1.0
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
        , entryDirection = Just OrthogonalDirection.Up
        }



--
-- Nature tiles
--


natureTopLeftCorner : TileConfig.SingleTile
natureTopLeftCorner =
    { id = 90
    , name = "_subgrid"
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
    { id = 91
    , name = "_subgrid"
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
    { id = 92
    , name = "_subgrid"
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
    { id = 93
    , name = "_subgrid"
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


natureEndUp : TileConfig.SingleTile
natureEndUp =
    { id = 94
    , name = "_subgrid"
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = Green
        , right = Green
        , bottom = largeTileInnerEdgeSocket
        , left = Green
        }
    , baseTileId = Nothing
    }


natureEndDown : TileConfig.SingleTile
natureEndDown =
    { id = 95
    , name = "_subgrid"
    , weight = defaultWeight
    , graphPriority = maxGraphPriority
    , biome = TileConfig.Nature
    , sockets =
        { top = largeTileInnerEdgeSocket
        , right = Green
        , bottom = Green
        , left = Green
        }
    , baseTileId = Nothing
    }


natureSingle1 : TileConfig
natureSingle1 =
    TileConfig.Single
        { id = 200
        , name = "NatureSingle1"
        , weight = 0.3
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


natureSingle2 : TileConfig
natureSingle2 =
    TileConfig.Single
        { id = 201
        , name = "NatureSingle2"
        , weight = 0.3
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


natureSingle3 : TileConfig
natureSingle3 =
    TileConfig.Single
        { id = 202
        , name = "NatureSingle3"
        , weight = 0.2
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


natureSingle4 : TileConfig
natureSingle4 =
    TileConfig.Single
        { id = 203
        , name = "NatureSingle4"
        , weight = 0.2
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


natureDouble1 : TileConfig
natureDouble1 =
    TileConfig.Large
        { id = 204
        , name = "NatureDouble1"
        , weight = 0.3
        , biome = TileConfig.Nature
        , tiles =
            Array.fromList
                [ natureEndUp

                --
                , natureEndDown
                ]
        , width = 1
        , height = 2
        , anchorIndex = 0
        , entryDirection = Nothing
        }


natureDouble2 : TileConfig
natureDouble2 =
    TileConfig.Large
        { id = 205
        , name = "NatureDouble2"
        , weight = 0.4
        , biome = TileConfig.Nature
        , tiles =
            Array.fromList
                [ natureEndUp

                --
                , natureEndDown
                ]
        , width = 1
        , height = 2
        , anchorIndex = 0
        , entryDirection = Nothing
        }


natureQuad1 : TileConfig
natureQuad1 =
    TileConfig.Large
        { id = 206
        , name = "NatureQuad1"
        , weight = 0.3
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
        , entryDirection = Nothing
        }


natureQuad2 : TileConfig
natureQuad2 =
    TileConfig.Large
        { id = 207
        , name = "NatureQuad2"
        , weight = 0.4
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
        , entryDirection = Nothing
        }
