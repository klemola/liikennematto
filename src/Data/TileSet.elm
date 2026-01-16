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
    [ loneRoad
    , horizontalRoad
    , verticalRoad
    , deadendUp
    , deadendRight
    , deadendDown
    , deadendLeft
    , curveBottomRight
    , curveBottomLeft
    , curveTopLeft
    , curveTopRight
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
    , curveTopLeftLotEntryLeft
    , curveTopLeftLotEntryUp
    , curveTopRightLotEntryRight
    , curveTopRightLotEntryUp
    , curveBottomRightLotEntryRight
    , curveBottomLeftLotEntryLeft
    , intersectionTDownLotEntryUp
    , intersectionTLeftLotEntryRight
    , intersectionTRightLotEntryLeft

    --
    , twoByTwoLotRight
    , threeByThreeLotLeft
    , threeByTwoLotUp
    , threeByTwoLotLeft
    , twoByThreeLotUp
    , twoByTwoLotLeft
    , threeByTwoLotRight
    , threeByThreeLotRight

    --
    , natureSingle1
    , natureSingle2
    , natureSingle3
    , natureSingle4
    , natureDouble1
    , natureDouble2
    , natureQuad1
    , natureQuad2
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
-- ID 0 is reserved for "no tile"


loneRoad : TileConfig
loneRoad =
    TileConfig.Single
        { id = 1
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
        { id = 2
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
    rotatedClockwise 3 "RoadVertical" horizontalRoad


deadendUp : TileConfig
deadendUp =
    TileConfig.Single
        { id = 4
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
    rotatedClockwise 5 "RoadDeadendRight" deadendUp


deadendDown : TileConfig
deadendDown =
    rotatedClockwise 6 "RoadDeadendDown" deadendRight


deadendLeft : TileConfig
deadendLeft =
    rotatedClockwise 7 "RoadDeadendLeft" deadendDown


curveBottomRight : TileConfig
curveBottomRight =
    TileConfig.Single
        { id = 8
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
    rotatedClockwise 9 "RoadCurveBottomLeft" curveBottomRight


curveTopLeft : TileConfig
curveTopLeft =
    rotatedClockwise 10 "RoadCurveTopLeft" curveBottomLeft


curveTopRight : TileConfig
curveTopRight =
    rotatedClockwise 11 "RoadCurveTopRight" curveTopLeft



-- T-intersections cannot be simply rotations of each other, as they have asymmetric sockets (crossings are not on both sides)


intersectionTUp : TileConfig
intersectionTUp =
    TileConfig.Single
        { id = 12
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
    mirroredVertically 13 "RoadIntersectionTDown" intersectionTUp


intersectionTLeft : TileConfig
intersectionTLeft =
    TileConfig.Single
        { id = 14
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
    mirroredHorizontally 15 "RoadIntersectionTRight" intersectionTLeft


intersectionCross : TileConfig
intersectionCross =
    TileConfig.Single
        { id = 16
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
        { id = 17
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
        , baseTileId = Just (TileConfig.tileConfigId horizontalRoad)
        }



-- ID 18 reserved for horizontalRoadLotEntryDown


verticalRoadLotEntryRight : TileConfig
verticalRoadLotEntryRight =
    TileConfig.Single
        { id = 19
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
        , baseTileId = Just (TileConfig.tileConfigId verticalRoad)
        }


verticalRoadLotEntryLeft : TileConfig
verticalRoadLotEntryLeft =
    TileConfig.Single
        { id = 20
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
        , baseTileId = Just (TileConfig.tileConfigId verticalRoad)
        }


deadendLeftLotEntryUp : TileConfig
deadendLeftLotEntryUp =
    TileConfig.Single
        { id = 21
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
        , baseTileId = Just (TileConfig.tileConfigId deadendLeft)
        }


deadendLeftLotEntryLeft : TileConfig
deadendLeftLotEntryLeft =
    TileConfig.Single
        { id = 22
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
        , baseTileId = Just (TileConfig.tileConfigId deadendLeft)
        }



-- ID 23 reserved for deadendLeftLotEntryDown


deadendRightLotEntryUp : TileConfig
deadendRightLotEntryUp =
    TileConfig.Single
        { id = 24
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
        , baseTileId = Just (TileConfig.tileConfigId deadendRight)
        }


deadendRightLotEntryRight : TileConfig
deadendRightLotEntryRight =
    TileConfig.Single
        { id = 25
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
        , baseTileId = Just (TileConfig.tileConfigId deadendRight)
        }



-- ID 26 reserved for deadendRightLotEntryDown


deadendDownLotEntryLeft : TileConfig
deadendDownLotEntryLeft =
    TileConfig.Single
        { id = 27
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
        , baseTileId = Just (TileConfig.tileConfigId deadendDown)
        }


deadendDownLotEntryRight : TileConfig
deadendDownLotEntryRight =
    TileConfig.Single
        { id = 28
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
        , baseTileId = Just (TileConfig.tileConfigId deadendDown)
        }



-- ID 29 reserved for deadendDownLotEntryDown


deadendUpLotEntryLeft : TileConfig
deadendUpLotEntryLeft =
    TileConfig.Single
        { id = 30
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
        , baseTileId = Just (TileConfig.tileConfigId deadendUp)
        }


deadendUpLotEntryRight : TileConfig
deadendUpLotEntryRight =
    TileConfig.Single
        { id = 31
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
        , baseTileId = Just (TileConfig.tileConfigId deadendUp)
        }


deadendUpLotEntryUp : TileConfig
deadendUpLotEntryUp =
    TileConfig.Single
        { id = 32
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
        , baseTileId = Just (TileConfig.tileConfigId deadendUp)
        }


curveBottomRightLotEntryRight : TileConfig
curveBottomRightLotEntryRight =
    TileConfig.Single
        { id = 33
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
        , baseTileId = Just (TileConfig.tileConfigId curveBottomRight)
        }



-- ID 34 reserved for curveBottomRightLotEntryDown


curveBottomLeftLotEntryLeft : TileConfig
curveBottomLeftLotEntryLeft =
    TileConfig.Single
        { id = 35
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
        , baseTileId = Just (TileConfig.tileConfigId curveBottomLeft)
        }



-- ID 36 reserved for curveBottomLeftLotEntryDown


curveTopLeftLotEntryLeft : TileConfig
curveTopLeftLotEntryLeft =
    TileConfig.Single
        { id = 37
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
        , baseTileId = Just (TileConfig.tileConfigId curveTopLeft)
        }


curveTopLeftLotEntryUp : TileConfig
curveTopLeftLotEntryUp =
    TileConfig.Single
        { id = 38
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
        , baseTileId = Just (TileConfig.tileConfigId curveTopLeft)
        }


curveTopRightLotEntryRight : TileConfig
curveTopRightLotEntryRight =
    TileConfig.Single
        { id = 39
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
        , baseTileId = Just (TileConfig.tileConfigId curveTopRight)
        }


curveTopRightLotEntryUp : TileConfig
curveTopRightLotEntryUp =
    TileConfig.Single
        { id = 40
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
        , baseTileId = Just (TileConfig.tileConfigId curveTopRight)
        }


intersectionTDownLotEntryUp : TileConfig
intersectionTDownLotEntryUp =
    TileConfig.Single
        { id = 41
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
        , baseTileId = Just (TileConfig.tileConfigId intersectionTDown)
        }


intersectionTRightLotEntryLeft : TileConfig
intersectionTRightLotEntryLeft =
    TileConfig.Single
        { id = 42
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
        , baseTileId = Just (TileConfig.tileConfigId intersectionTRight)
        }


intersectionTLeftLotEntryRight : TileConfig
intersectionTLeftLotEntryRight =
    TileConfig.Single
        { id = 43
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
        , baseTileId = Just (TileConfig.tileConfigId intersectionTLeft)
        }



-- ID 44 reserved for intersectionTUpLotEntryDown
--
-- Lot tiles
--


lotDrivewayTileIds : List TileId
lotDrivewayTileIds =
    [ lotDrivewayLeft, lotDrivewayRight, lotDrivewayUp ]
        |> List.map .id


lotTopLeftCorner : TileConfig.SingleTile
lotTopLeftCorner =
    { id = 100
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
    { id = 101
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
    { id = 102
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
    { id = 103
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
    { id = 104
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
    { id = 105
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
    { id = 106
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
    { id = 107
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
    { id = 108
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
    { id = 109
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
    { id = 110
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
    { id = 111
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



--- ID 112 reserved for lotDrivewayDown


twoByTwoLotRight : TileConfig
twoByTwoLotRight =
    TileConfig.Large twoByTwoLotRightLargeTile


twoByTwoLotRightLargeTile : LargeTile
twoByTwoLotRightLargeTile =
    { id = 113
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
    { id = 114
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
    { id = 115
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
        { id = 116
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
        { id = 117
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


twoByTwoLotLeft : TileConfig
twoByTwoLotLeft =
    TileConfig.Large twoByTwoLotLeftLargeTile


twoByTwoLotLeftLargeTile : LargeTile
twoByTwoLotLeftLargeTile =
    { id = 118
    , name = "TwoByTwoLotLeft"
    , weight = 1.0
    , biome = TileConfig.Lot
    , tiles =
        Array.fromList
            [ lotTopLeftCorner
            , lotTopRightCorner

            --
            , lotDrivewayLeft
            , lotBottomRightCorner
            ]
    , width = 2
    , height = 2
    , anchorIndex = 2
    , entryDirection = Just OrthogonalDirection.Right
    }


threeByTwoLotRight : TileConfig
threeByTwoLotRight =
    TileConfig.Large
        { id = 119
        , name = "ThreeByTwoLotRight"
        , weight = 1.0
        , biome = TileConfig.Lot
        , tiles =
            Array.fromList
                [ lotTopLeftCorner
                , lotTopEdge
                , lotTopRightCorner

                --
                , lotBottomLeftCorner
                , lotBottomEdge
                , lotDrivewayRight
                ]
        , width = 3
        , height = 2
        , anchorIndex = 5
        , entryDirection = Just OrthogonalDirection.Left
        }


threeByThreeLotRight : TileConfig
threeByThreeLotRight =
    TileConfig.Large threeByThreeLotRightLargeTile


threeByThreeLotRightLargeTile : LargeTile
threeByThreeLotRightLargeTile =
    { id = 120
    , name = "ThreeByThreeLotRight"
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
            , lotBottomLeftCorner
            , lotBottomEdge
            , lotDrivewayRight
            ]
    , width = 3
    , height = 3
    , anchorIndex = 8
    , entryDirection = Just OrthogonalDirection.Left
    }



--
-- Nature tiles
--


natureTopLeftCorner : TileConfig.SingleTile
natureTopLeftCorner =
    { id = 200
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
    { id = 201
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
    { id = 202
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
    { id = 203
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



-- ID 204, 205, 206, 207, 208 reserved for 3x3+ nature tiles


natureEndUp : TileConfig.SingleTile
natureEndUp =
    { id = 209
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
    { id = 210
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
        { id = 211
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
        { id = 212
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
        { id = 213
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
        { id = 214
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
        { id = 215
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
        { id = 216
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
        { id = 217
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
        { id = 218
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
