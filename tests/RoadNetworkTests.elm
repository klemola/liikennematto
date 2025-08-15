module RoadNetworkTests exposing (suite)

import Data.Utility
    exposing
        ( placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , tilemapFromCoordinates
        , worldFromTilemap
        )
import Data.Worlds as Worlds
import Dict exposing (Dict)
import Expect
import Length
import Lib.Collection exposing (Id)
import Model.World as World exposing (World)
import Point2d
import Simulation.RoadNetwork as RoadNetwork
import Test exposing (Test, describe, test)
import Tilemap.Cell exposing (CellCoordinates)
import Tilemap.Core exposing (Tilemap, createTilemap)
import Tilemap.Tile as Tile


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap tenByTenTilemap (\_ -> Tile.init Tile.Unintialized)


suite : Test
suite =
    describe "Road Network Connection Validation"
        [ basicRoadPatternsTests
        , withLotsTests
        , tilemapBoundaryTests
        , intersectionConnectivityTests
        ]


basicRoadPatternsTests : Test
basicRoadPatternsTests =
    describe "Basic road patterns"
        [ test "T-intersection has correct connections"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 6
                            |> Expect.onFail "Expected 6 LaneConnector nodes for T-intersection (3 directions × 2 lanes)"
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                            |> Expect.onFail "Expected no validation errors"
                    ]
                    ()
            )
        , test "L-shaped road has correct connections at corner"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 3, 5 ), ( 4, 5 ), ( 4, 6 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    allNodes =
                        debugData.nodes
                in
                Expect.all
                    [ \_ ->
                        List.length allNodes
                            |> Expect.atLeast 6
                            |> Expect.onFail "Expected at least 6 nodes for L-shaped road"
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                            |> Expect.onFail "Expected no validation errors"
                    ]
                    ()
            )
        , test "four-way intersection has correct connections"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ), ( 6, 5 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 8
                            |> Expect.onFail "Expected 8 LaneConnector nodes for 4-way intersection (4 directions × 2 lanes)"
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                            |> Expect.onFail "Expected no validation errors"
                    ]
                    ()
            )
        , test "two-tile road creates deadend connections"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 5, 5 ), ( 6, 5 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    deadendNodes =
                        filterDeadendNodes debugData
                in
                Expect.all
                    [ \_ ->
                        List.length deadendNodes
                            |> Expect.equal 4
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                    ]
                    ()
            )
        , test "does not create duplicate LaneConnector nodes"
            (\_ ->
                let
                    world =
                        tilemapFromCoordinates tenByTenTilemap [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ) ]
                            |> worldFromTilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    nodePositions =
                        List.map
                            (\nodeCtx ->
                                Point2d.toTuple Length.inMeters nodeCtx.node.label.position
                            )
                            debugData.nodes

                    uniquePositions =
                        nodePositions
                            |> List.foldl
                                (\pos acc ->
                                    if List.member pos acc then
                                        acc

                                    else
                                        pos :: acc
                                )
                                []
                in
                List.length nodePositions
                    |> Expect.equal (List.length uniquePositions)
                    |> Expect.onFail "Expected no duplicate node positions"
            )
        ]


withLotsTests : Test
withLotsTests =
    describe "With lots"
        [ test "world with school has correct connections"
            (\_ ->
                let
                    world =
                        Worlds.worldWithSchool
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    lotEntryNodes =
                        filterLotEntryNodes debugData

                    lotExitNodes =
                        filterLotExitNodes debugData
                in
                Expect.all
                    [ \_ ->
                        List.length lotEntryNodes
                            |> Expect.equal 1
                    , \_ ->
                        List.length lotExitNodes
                            |> Expect.equal 1
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                    ]
                    ()
            )
        ]


tilemapBoundaryTests : Test
tilemapBoundaryTests =
    describe "Tilemap boundaries"
        [ test "roads at tilemap edges have correct connections"
            (\_ ->
                let
                    world =
                        tilemapFromCoordinates tenByTenTilemap [ ( 1, 5 ), ( 2, 5 ) ]
                            |> worldFromTilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork
                in
                Expect.all
                    [ \_ ->
                        List.length debugData.nodes
                            |> Expect.atLeast 2
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                    ]
                    ()
            )
        , test "long distance straight road maintains connections"
            (\_ ->
                let
                    longRoadCells =
                        List.range 1 10 |> List.map (\x -> ( x, 5 ))

                    tilemap =
                        placeRoadAndUpdateBuffer longRoadCells emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    deadendNodes =
                        filterDeadendNodes debugData
                in
                Expect.all
                    [ \_ ->
                        List.length deadendNodes
                            |> Expect.equal 4
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                    ]
                    ()
            )
        , test "disconnected road segments have no cross-connections"
            (\_ ->
                let
                    world =
                        tilemapFromCoordinates tenByTenTilemap [ ( 2, 2 ), ( 3, 2 ), ( 7, 7 ), ( 8, 7 ) ]
                            |> worldFromTilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    connections =
                        connectionsByNodeId debugData

                    connectedComponents =
                        Dict.size connections
                in
                Expect.all
                    [ \_ ->
                        connectedComponents
                            |> Expect.atLeast 2
                    , \_ ->
                        validateAllConnections world
                            |> List.isEmpty
                            |> Expect.equal True
                    ]
                    ()
            )
        ]


intersectionConnectivityTests : Test
intersectionConnectivityTests =
    describe "Intersection connectivity"
        [ test "T-intersection nodes connect to each other correctly"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    connections =
                        connectionsByNodeId debugData

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData

                    nodesWithoutConnections =
                        findNodesWithoutConnections connections laneConnectorNodes
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 6
                    , \_ ->
                        List.length nodesWithoutConnections
                            |> Expect.equal 0
                            |> Expect.onFail ("Expected all LaneConnector nodes to have outgoing connections, but " ++ String.fromInt (List.length nodesWithoutConnections) ++ " nodes have no connections")
                    ]
                    ()
            )
        , test "four-way intersection nodes connect to each other properly"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 4, 5 ), ( 5, 4 ), ( 5, 5 ), ( 5, 6 ), ( 6, 5 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    connections =
                        connectionsByNodeId debugData

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData

                    nodesWithoutConnections =
                        findNodesWithoutConnections connections laneConnectorNodes
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 8
                    , \_ ->
                        List.length nodesWithoutConnections
                            |> Expect.equal 0
                            |> Expect.onFail ("Expected all LaneConnector nodes to have outgoing connections, but " ++ String.fromInt (List.length nodesWithoutConnections) ++ " nodes have no connections")
                    ]
                    ()
            )
        , test "L-shaped curve nodes connect to each other properly"
            (\_ ->
                let
                    tilemap =
                        placeRoadAndUpdateBuffer [ ( 3, 5 ), ( 4, 5 ), ( 4, 6 ) ] emptyTilemap

                    world =
                        worldFromTilemap tilemap
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    connections =
                        connectionsByNodeId debugData

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData

                    nodesWithoutConnections =
                        findNodesWithoutConnections connections laneConnectorNodes
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 4
                    , \_ ->
                        List.length nodesWithoutConnections
                            |> Expect.equal 0
                            |> Expect.onFail ("Expected all LaneConnector nodes to have outgoing connections, but " ++ String.fromInt (List.length nodesWithoutConnections) ++ " nodes have no connections")
                    ]
                    ()
            )
        , test "intersection with lot connects properly"
            (\_ ->
                let
                    world =
                        Worlds.worldWithSchool
                            |> World.updateRoadNetwork

                    debugData =
                        RoadNetwork.toDebugData world.roadNetwork

                    connections =
                        connectionsByNodeId debugData

                    laneConnectorNodes =
                        filterLaneConnectorNodes debugData

                    lotNodes =
                        filterLotNodes debugData

                    allRelevantNodes =
                        laneConnectorNodes ++ lotNodes

                    nodesWithoutConnections =
                        findNodesWithoutConnections connections allRelevantNodes
                in
                Expect.all
                    [ \_ ->
                        List.length laneConnectorNodes
                            |> Expect.equal 6
                    , \_ ->
                        List.length lotNodes
                            |> Expect.equal 2
                    , \_ ->
                        List.length nodesWithoutConnections
                            |> Expect.equal 1
                            |> Expect.onFail ("Expected only one node without connections (lot entry), but " ++ String.fromInt (List.length nodesWithoutConnections) ++ " nodes have no connections")
                    ]
                    ()
            )
        ]



-- Helpers


type ValidationError
    = MissingLotConnection Id CellCoordinates
    | SpatialIndexingError String CellCoordinates


validateAllConnections : World -> List ValidationError
validateAllConnections world =
    let
        debugData =
            RoadNetwork.toDebugData world.roadNetwork

        lotConnectionErrors =
            validateLotConnections debugData

        spatialIndexingErrors =
            validateSpatialIndexing debugData
    in
    lotConnectionErrors ++ spatialIndexingErrors


validateLotConnections : RoadNetwork.DebugData -> List ValidationError
validateLotConnections debugData =
    let
        lotIds =
            List.filterMap
                (\nodeCtx ->
                    case nodeCtx.node.label.kind of
                        RoadNetwork.LotEntry lotId ->
                            Just lotId

                        RoadNetwork.LotExit lotId ->
                            Just lotId

                        _ ->
                            Nothing
                )
                debugData.nodes
                |> List.foldl (\lotId acc -> Dict.insert (Lib.Collection.idToString lotId) lotId acc) Dict.empty

        hasEntryForLot lotId =
            List.any
                (\nodeCtx ->
                    case nodeCtx.node.label.kind of
                        RoadNetwork.LotEntry id ->
                            id == lotId

                        _ ->
                            False
                )
                debugData.nodes

        hasExitForLot lotId =
            List.any
                (\nodeCtx ->
                    case nodeCtx.node.label.kind of
                        RoadNetwork.LotExit id ->
                            id == lotId

                        _ ->
                            False
                )
                debugData.nodes
    in
    Dict.foldl
        (\_ lotId acc ->
            if hasEntryForLot lotId && hasExitForLot lotId then
                acc

            else
                MissingLotConnection lotId ( 0, 0 ) :: acc
        )
        []
        lotIds


validateSpatialIndexing : RoadNetwork.DebugData -> List ValidationError
validateSpatialIndexing debugData =
    let
        positionCounts =
            List.foldl
                (\nodeCtx acc ->
                    let
                        position =
                            Point2d.toTuple Length.inMeters nodeCtx.node.label.position
                    in
                    Dict.update position
                        (\maybeCount ->
                            case maybeCount of
                                Just count ->
                                    Just (count + 1)

                                Nothing ->
                                    Just 1
                        )
                        acc
                )
                Dict.empty
                debugData.nodes
    in
    Dict.foldl
        (\position count acc ->
            if count > 1 then
                SpatialIndexingError
                    ("Duplicate nodes at position " ++ Debug.toString position)
                    ( 0, 0 )
                    :: acc

            else
                acc
        )
        []
        positionCounts


connectionsByNodeId : RoadNetwork.DebugData -> Dict Int (List Int)
connectionsByNodeId debugData =
    List.foldl
        (\edge acc ->
            Dict.update edge.fromNodeId
                (\maybeConnections ->
                    case maybeConnections of
                        Just connections ->
                            Just (edge.toNodeId :: connections)

                        Nothing ->
                            Just [ edge.toNodeId ]
                )
                acc
        )
        Dict.empty
        debugData.edges


filterLaneConnectorNodes : RoadNetwork.DebugData -> List RoadNetwork.RNNodeContext
filterLaneConnectorNodes debugData =
    List.filter
        (\nodeCtx ->
            case nodeCtx.node.label.kind of
                RoadNetwork.LaneConnector ->
                    True

                _ ->
                    False
        )
        debugData.nodes


filterDeadendNodes : RoadNetwork.DebugData -> List RoadNetwork.RNNodeContext
filterDeadendNodes debugData =
    List.filter
        (\nodeCtx ->
            case nodeCtx.node.label.kind of
                RoadNetwork.DeadendEntry ->
                    True

                RoadNetwork.DeadendExit ->
                    True

                _ ->
                    False
        )
        debugData.nodes


filterLotEntryNodes : RoadNetwork.DebugData -> List RoadNetwork.RNNodeContext
filterLotEntryNodes debugData =
    List.filter
        (\nodeCtx ->
            case nodeCtx.node.label.kind of
                RoadNetwork.LotEntry _ ->
                    True

                _ ->
                    False
        )
        debugData.nodes


filterLotExitNodes : RoadNetwork.DebugData -> List RoadNetwork.RNNodeContext
filterLotExitNodes debugData =
    List.filter
        (\nodeCtx ->
            case nodeCtx.node.label.kind of
                RoadNetwork.LotExit _ ->
                    True

                _ ->
                    False
        )
        debugData.nodes


filterLotNodes : RoadNetwork.DebugData -> List RoadNetwork.RNNodeContext
filterLotNodes debugData =
    List.filter
        (\nodeCtx ->
            case nodeCtx.node.label.kind of
                RoadNetwork.LotEntry _ ->
                    True

                RoadNetwork.LotExit _ ->
                    True

                _ ->
                    False
        )
        debugData.nodes


findNodesWithoutConnections : Dict.Dict Int (List Int) -> List RoadNetwork.RNNodeContext -> List RoadNetwork.RNNodeContext
findNodesWithoutConnections connections nodes =
    List.filter
        (\nodeCtx ->
            case Dict.get nodeCtx.node.id connections of
                Just nodeConnections ->
                    List.isEmpty nodeConnections

                Nothing ->
                    True
        )
        nodes
