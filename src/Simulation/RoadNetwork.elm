module Simulation.RoadNetwork exposing
    ( Connection
    , ConnectionEnvironment(..)
    , ConnectionKind(..)
    , Lane
    , RNNode
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , buildRoadNetwork
    , empty
    , findLotExitNodeByLotId
    , getOutgoingConnectionIds
    , getOutgoingConnectionsAndCosts
    , getRandomNode
    , nodeById
    , nodeByPosition
    , nodeDirection
    , nodeLotId
    , nodePosition
    , nodeTrafficControl
    , size
    , toRoadConnectionPoints
    )

import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates, LocalCoordinates)
import Data.Lots exposing (drivewayOffset)
import Data.TileSet
    exposing
        ( basicRoadTiles
        , connectionsByTile
        , tileById
        )
import Dict exposing (Dict)
import Direction2d exposing (Direction2d)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection, Id, idMatches)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import List.Extra
import Maybe.Extra as Maybe
import Point2d exposing (Point2d)
import Quantity
import Random
import Random.Extra
import Set
import Simulation.TrafficLight as TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( TileListFilter(..)
        , Tilemap
        , TilemapConfig
        , fixedTileByCell
        , getTilemapConfig
        , tileToConfig
        , tilemapToList
        )
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig as TileConfig exposing (TileConfig)
import Vector2d exposing (Vector2d)


type alias RoadNetwork =
    Graph Connection Lane


type alias RNNodeContext =
    NodeContext Connection Lane


type alias RNNode =
    Graph.Node Connection


type alias Connection =
    { kind : ConnectionKind
    , position : Point2d Length.Meters GlobalCoordinates
    , direction : Direction2d GlobalCoordinates
    , environment : ConnectionEnvironment
    , cell : Cell
    , trafficControl : TrafficControl
    }


type ConnectionKind
    = LaneConnector
    | DeadendEntry
    | DeadendExit
    | LotEntry Id
    | LotExit Id


type ConnectionEnvironment
    = Road
    | Intersection Int


type TrafficControl
    = Signal Id
    | Yield (BoundingBox2d Length.Meters GlobalCoordinates)
    | NoTrafficControl


type alias Lane =
    Length


type alias TrafficLights =
    Collection TrafficLight


empty : RoadNetwork
empty =
    Graph.empty


innerLaneOffset : Length
innerLaneOffset =
    -- the distance from a road tile's edge to the inner lane (from left / bottom side)
    Length.meters 6


outerLaneOffset : Length
outerLaneOffset =
    -- the distance from a road tile's edge to the outer lane (from the left / bottom side)
    Length.meters 10


laneStartOffsetUp : Vector2d Length.Meters coordinates
laneStartOffsetUp =
    Vector2d.xy outerLaneOffset Cell.size


laneEndOffsetUp : Vector2d Length.Meters coordinates
laneEndOffsetUp =
    Vector2d.xy innerLaneOffset Cell.size


laneStartOffsetRight : Vector2d Length.Meters coordinates
laneStartOffsetRight =
    Vector2d.xy Cell.size innerLaneOffset


laneEndOffsetRight : Vector2d Length.Meters coordinates
laneEndOffsetRight =
    Vector2d.xy Cell.size outerLaneOffset


laneStartOffsetDown : Vector2d Length.Meters coordinates
laneStartOffsetDown =
    Vector2d.xy innerLaneOffset Quantity.zero


laneEndOffsetDown : Vector2d Length.Meters coordinates
laneEndOffsetDown =
    Vector2d.xy outerLaneOffset Quantity.zero


laneStartOffsetLeft : Vector2d Length.Meters coordinates
laneStartOffsetLeft =
    Vector2d.xy Quantity.zero outerLaneOffset


laneEndOffsetLeft : Vector2d Length.Meters coordinates
laneEndOffsetLeft =
    Vector2d.xy Quantity.zero innerLaneOffset


toRoadConnectionPoints : OrthogonalDirection -> Length -> ( Point2d Length.Meters LocalCoordinates, Point2d Length.Meters LocalCoordinates )
toRoadConnectionPoints entryDirection lotWidth =
    case entryDirection of
        Right ->
            ( Point2d.xy
                Quantity.zero
                (Cell.size
                    |> Quantity.minus outerLaneOffset
                    |> Quantity.minus drivewayOffset
                )
            , Point2d.xy
                Quantity.zero
                (Cell.size
                    |> Quantity.minus innerLaneOffset
                    |> Quantity.minus drivewayOffset
                )
            )

        Up ->
            ( Point2d.xy
                (outerLaneOffset |> Quantity.minus drivewayOffset)
                Quantity.zero
            , Point2d.xy
                (innerLaneOffset |> Quantity.minus drivewayOffset)
                Quantity.zero
            )

        Left ->
            ( Point2d.xy
                lotWidth
                (Cell.size
                    |> Quantity.minus innerLaneOffset
                    |> Quantity.minus drivewayOffset
                )
            , Point2d.xy
                lotWidth
                (Cell.size
                    |> Quantity.minus outerLaneOffset
                    |> Quantity.minus drivewayOffset
                )
            )

        _ ->
            ( Point2d.origin, Point2d.origin )



--
-- Queries
--


size : RoadNetwork -> Int
size =
    Graph.size


findLotExitNodeByLotId : RoadNetwork -> Id -> Maybe RNNodeContext
findLotExitNodeByLotId roadNetwork lotId =
    Graph.fold
        (\ctx acc ->
            case ctx.node.label.kind of
                LotExit id ->
                    if idMatches id lotId then
                        Just ctx

                    else
                        acc

                _ ->
                    acc
        )
        Nothing
        roadNetwork


nodeById : RoadNetwork -> NodeId -> Maybe RNNodeContext
nodeById roadNetwork nodeId =
    Graph.get nodeId roadNetwork


nodeByPosition : RoadNetwork -> Point2d Length.Meters GlobalCoordinates -> Maybe RNNodeContext
nodeByPosition roadNetwork position =
    Graph.nodes roadNetwork
        |> List.filterMap
            (\{ id, label } ->
                if label.position == position then
                    Just id

                else
                    Nothing
            )
        |> List.head
        |> Maybe.andThen (\matchId -> Graph.get matchId roadNetwork)


getRandomNode : RoadNetwork -> Random.Seed -> (RNNode -> Bool) -> ( Maybe RNNodeContext, Random.Seed )
getRandomNode roadNetwork seed predicate =
    let
        randomNodeGenerator =
            roadNetwork
                |> Graph.nodes
                |> List.filterMap
                    (\node ->
                        if predicate node then
                            Just node.id

                        else
                            Nothing
                    )
                |> Random.Extra.sample
                |> Random.map (Maybe.andThen (nodeById roadNetwork))
    in
    Random.step randomNodeGenerator seed


getOutgoingConnectionIds : RNNodeContext -> List NodeId
getOutgoingConnectionIds nodeCtx =
    Graph.alongOutgoingEdges nodeCtx


getOutgoingConnectionsAndCosts : RoadNetwork -> RNNodeContext -> List ( RNNodeContext, Length )
getOutgoingConnectionsAndCosts roadNetwork nodeCtx =
    IntDict.foldl
        (\k lane acc ->
            case nodeById roadNetwork k of
                Just connection ->
                    ( connection, lane ) :: acc

                Nothing ->
                    acc
        )
        []
        nodeCtx.outgoing


nodeTrafficControl : RNNodeContext -> TrafficControl
nodeTrafficControl nodeCtx =
    nodeCtx.node.label.trafficControl


nodeLotId : RNNodeContext -> Maybe Id
nodeLotId nodeCtx =
    case nodeCtx.node.label.kind of
        LotEntry id ->
            Just id

        LotExit id ->
            Just id

        _ ->
            Nothing


nodePosition : RNNodeContext -> Point2d Length.Meters GlobalCoordinates
nodePosition nodeCtx =
    nodeCtx.node.label.position


nodeDirection : RNNodeContext -> Direction2d GlobalCoordinates
nodeDirection nodeCtx =
    nodeCtx.node.label.direction


connectionPosition : Node Connection -> Point2d Length.Meters GlobalCoordinates
connectionPosition node =
    node.label.position


connectionDirection : Node Connection -> Direction2d GlobalCoordinates
connectionDirection node =
    node.label.direction


buildRoadNetwork : Tilemap -> Dict CellCoordinates ( Id, OrthogonalDirection ) -> TrafficLights -> ( RoadNetwork, TrafficLights )
buildRoadNetwork tilemap lotEntries trafficLights =
    let
        tilePriority ( _, tile ) =
            case tileToConfig tile of
                Just tileConfig ->
                    TileConfig.graphPriority tileConfig

                Nothing ->
                    TileConfig.maxGraphPriority

        ( nodes, spatialIndex ) =
            createConnections
                { tilemap = tilemap
                , lotEntries = lotEntries
                , nodes = Dict.empty
                , spatialIndex = Dict.empty
                , remainingTiles =
                    tilemap
                        |> tilemapToList (\cell tile -> Just ( cell, tile )) NoFilter
                        |> List.sortBy tilePriority
                }

        edges =
            createLanes (getTilemapConfig tilemap) spatialIndex (Dict.values nodes)

        roadNetwork =
            Graph.fromNodesAndEdges (Dict.values nodes) edges
    in
    roadNetwork |> setupTrafficControl trafficLights



--
-- Connections
--


type alias CellIndex =
    Dict CellCoordinates (List (Node Connection))


type alias NodesMemo =
    Dict ( Float, Float ) (Node Connection)


createConnections :
    { tilemap : Tilemap
    , lotEntries : Dict CellCoordinates ( Id, OrthogonalDirection )
    , nodes : NodesMemo
    , spatialIndex : CellIndex
    , remainingTiles : List ( Cell, Tile )
    }
    -> ( NodesMemo, CellIndex )
createConnections { nodes, spatialIndex, tilemap, lotEntries, remainingTiles } =
    case remainingTiles of
        [] ->
            ( nodes, spatialIndex )

        ( cell, tile ) :: otherTiles ->
            let
                connectionsInTile =
                    toConnections tilemap lotEntries cell tile

                ( updatedNodes, newNodes ) =
                    List.foldl
                        (\nodeSpec ( currentNodes, accNewNodes ) ->
                            let
                                key =
                                    Point2d.toTuple Length.inMeters nodeSpec.position
                            in
                            if Dict.member key currentNodes then
                                ( currentNodes, accNewNodes )

                            else
                                let
                                    newNode =
                                        Node (Dict.size currentNodes) nodeSpec
                                in
                                ( Dict.insert key newNode currentNodes
                                , newNode :: accNewNodes
                                )
                        )
                        ( nodes, [] )
                        connectionsInTile

                updatedSpatialIndex =
                    List.foldl (addNodeToSpatialIndex tilemap) spatialIndex newNodes
            in
            createConnections
                { tilemap = tilemap
                , lotEntries = lotEntries
                , nodes = updatedNodes
                , spatialIndex = updatedSpatialIndex
                , remainingTiles = otherTiles
                }


addNodeToSpatialIndex : Tilemap -> Node Connection -> CellIndex -> CellIndex
addNodeToSpatialIndex tilemap node index =
    let
        indexWithCurrentCell =
            addNodeToCell (Cell.coordinates node.label.cell) node index
    in
    case node.label.kind of
        LaneConnector ->
            OrthogonalDirection.fromDirection2d node.label.direction
                |> Maybe.andThen
                    (\orthDir ->
                        let
                            dir =
                                if
                                    node.label.cell
                                        |> Cell.centerPoint
                                        |> Common.isInTheNormalPlaneOf node.label.direction node.label.position
                                then
                                    OrthogonalDirection.opposite orthDir

                                else
                                    orthDir
                        in
                        Cell.nextOrthogonalCell (getTilemapConfig tilemap) dir node.label.cell
                            |> Maybe.map
                                (\targetNeighbor ->
                                    addNodeToCell (Cell.coordinates targetNeighbor) node indexWithCurrentCell
                                )
                    )
                |> Maybe.withDefault indexWithCurrentCell

        _ ->
            indexWithCurrentCell


addNodeToCell : CellCoordinates -> Node Connection -> CellIndex -> CellIndex
addNodeToCell cellCoords node index =
    let
        existingNodes =
            Dict.get cellCoords index |> Maybe.withDefault []
    in
    Dict.insert cellCoords (node :: existingNodes) index


toConnections : Tilemap -> Dict CellCoordinates ( Id, OrthogonalDirection ) -> Cell -> Tile -> List Connection
toConnections tilemap lotEntries cell tile =
    case Tile.id tile of
        Just tileId ->
            let
                tileConfig =
                    tileById tileId

                connections =
                    connectionsByTile tileConfig

                combinedConnections =
                    case connections.lotConnection of
                        Just lotEntryDir ->
                            lotEntryDir :: connections.roadConnections

                        Nothing ->
                            connections.roadConnections
            in
            case combinedConnections of
                [] ->
                    []

                [ single ] ->
                    single
                        |> OrthogonalDirection.opposite
                        |> OrthogonalDirection.toDirection2d
                        |> deadendConnections cell

                multiple ->
                    if shouldIgnoreConnections tileConfig then
                        []

                    else
                        let
                            intersectionDirectionsAmount =
                                List.length connections.roadConnections

                            environment =
                                if intersectionDirectionsAmount >= 3 then
                                    Intersection intersectionDirectionsAmount

                                else
                                    Road
                        in
                        List.concatMap (connectionsByTileEntryDirection tilemap lotEntries cell environment tile) multiple

        Nothing ->
            []


deadendConnections : Cell -> Direction2d GlobalCoordinates -> List Connection
deadendConnections cell trafficDirection =
    let
        ( entryPosition, exitPosition ) =
            laneCenterPositionsByDirection cell trafficDirection

        entryConnection =
            { kind = DeadendEntry
            , position = entryPosition
            , direction = trafficDirection
            , environment = Road
            , cell = cell
            , trafficControl = NoTrafficControl
            }

        exitConnection =
            { kind = DeadendExit
            , position = exitPosition
            , direction = Direction2d.reverse trafficDirection
            , environment = Road
            , cell = cell
            , trafficControl = NoTrafficControl
            }
    in
    [ entryConnection
    , exitConnection
    ]


laneCenterPositionsByDirection : Cell -> Direction2d GlobalCoordinates -> ( Point2d Length.Meters GlobalCoordinates, Point2d Length.Meters GlobalCoordinates )
laneCenterPositionsByDirection cell trafficDirection =
    let
        connectionOffsetFromTileCenter =
            Cell.size
                |> Quantity.half
                |> Quantity.minus innerLaneOffset

        tileCenterPosition =
            Cell.centerPoint cell
    in
    ( tileCenterPosition
        |> Point2d.translateIn
            (Direction2d.rotateClockwise trafficDirection)
            connectionOffsetFromTileCenter
    , tileCenterPosition
        |> Point2d.translateIn
            (Direction2d.rotateCounterclockwise trafficDirection)
            connectionOffsetFromTileCenter
    )


connectionsByTileEntryDirection :
    Tilemap
    -> Dict CellCoordinates ( Id, OrthogonalDirection )
    -> Cell
    -> ConnectionEnvironment
    -> Tile
    -> OrthogonalDirection
    -> List Connection
connectionsByTileEntryDirection tilemap lotEntries cell environment tile direction =
    let
        origin =
            Cell.bottomLeftCorner cell

        lotEntry =
            Dict.get (Cell.coordinates cell) lotEntries

        awayFromTileDirection =
            OrthogonalDirection.toDirection2d direction

        facingTileDirection =
            Direction2d.reverse awayFromTileDirection

        ( startConnectionKind, endConnectionKind, extraOffset ) =
            case lotEntry of
                Just ( lotId, lotEntryDirection ) ->
                    if direction == lotEntryDirection then
                        ( LotEntry lotId, LotExit lotId, toLotOffset direction )

                    else
                        ( LaneConnector, LaneConnector, Vector2d.zero )

                Nothing ->
                    ( LaneConnector, LaneConnector, Vector2d.zero )

        ( startOffset, endOffset ) =
            connectionPositionByEntryDirection direction

        startConnectionCell =
            chooseConnectionCell tilemap tile direction startConnectionKind cell
    in
    [ { kind = startConnectionKind
      , position = origin |> Point2d.translateBy (startOffset |> Vector2d.plus extraOffset)
      , direction = awayFromTileDirection
      , environment = Road
      , cell = startConnectionCell
      , trafficControl = NoTrafficControl
      }
    , { kind = endConnectionKind
      , position = origin |> Point2d.translateBy (endOffset |> Vector2d.plus extraOffset)
      , direction = facingTileDirection
      , environment = environment
      , cell = cell
      , trafficControl = NoTrafficControl
      }
    ]


toLotOffset : OrthogonalDirection -> Vector2d Length.Meters coordinates
toLotOffset lotEntryDirection =
    case lotEntryDirection of
        Up ->
            Vector2d.xy (Quantity.negate drivewayOffset) Quantity.zero

        Right ->
            Vector2d.xy Quantity.zero (Quantity.negate drivewayOffset)

        Left ->
            Vector2d.xy Quantity.zero (Quantity.negate drivewayOffset)

        _ ->
            Vector2d.zero


connectionPositionByEntryDirection : OrthogonalDirection -> ( Vector2d Length.Meters coordinates, Vector2d Length.Meters coordinates )
connectionPositionByEntryDirection direction =
    case direction of
        Up ->
            ( laneStartOffsetUp, laneEndOffsetUp )

        Right ->
            ( laneStartOffsetRight, laneEndOffsetRight )

        Down ->
            ( laneStartOffsetDown, laneEndOffsetDown )

        Left ->
            ( laneStartOffsetLeft, laneEndOffsetLeft )


chooseConnectionCell :
    Tilemap
    -> Tile
    -> OrthogonalDirection
    -> ConnectionKind
    -> Cell
    -> Cell
chooseConnectionCell tilemap tile direction startConnectionKind baseCell =
    if startConnectionKind /= LaneConnector then
        baseCell

    else
        let
            tilemapConfig =
                getTilemapConfig tilemap
        in
        case Cell.nextOrthogonalCell tilemapConfig direction baseCell of
            Just nextCell ->
                if
                    fixedTileByCell tilemap nextCell
                        |> Maybe.unwrap False (hasOverlappingConnections tile)
                then
                    nextCell

                else
                    baseCell

            Nothing ->
                baseCell


hasOverlappingConnections : Tile -> Tile -> Bool
hasOverlappingConnections tileA tileB =
    hasConnectionsInMultipleDirections tileA && hasConnectionsInMultipleDirections tileB


hasConnectionsInMultipleDirections : Tile -> Bool
hasConnectionsInMultipleDirections tile =
    case Tile.id tile of
        Just tileId ->
            let
                tileConfig =
                    tileById tileId
            in
            not (shouldIgnoreConnections tileConfig) && List.length (connectionsByTile tileConfig |> .roadConnections) > 1

        Nothing ->
            False


shouldIgnoreConnections : TileConfig -> Bool
shouldIgnoreConnections tileConfig =
    -- "basic road tiles" should not create nodes, because they are always connected to a tile that has overlapping connections.
    Set.member (TileConfig.tileConfigId tileConfig) basicRoadTiles



--
-- Lanes
--


createLanes : TilemapConfig -> CellIndex -> List (Node Connection) -> List (Edge Lane)
createLanes tilemapConfig spatialIndex nodes =
    List.concatMap (toEdges tilemapConfig spatialIndex) nodes


toEdges : TilemapConfig -> CellIndex -> Node Connection -> List (Edge Lane)
toEdges tilemapConfig spatialIndex current =
    let
        potentialResultToEdges =
            Maybe.unwrap [] List.singleton

        completeLaneWithConfig =
            completeLane tilemapConfig spatialIndex

        matcherFn =
            case current.label.kind of
                DeadendEntry ->
                    connectDeadendEntryWithExit spatialIndex >> potentialResultToEdges

                DeadendExit ->
                    completeLaneWithConfig >> potentialResultToEdges

                LotEntry _ ->
                    always []

                LotExit _ ->
                    findLanesInsideCell spatialIndex

                LaneConnector ->
                    if
                        current.label.cell
                            |> Cell.centerPoint
                            |> Common.isInTheNormalPlaneOf current.label.direction current.label.position
                    then
                        findLanesInsideCell spatialIndex

                    else
                        -- the node is facing away from its' Cell
                        completeLaneWithConfig >> potentialResultToEdges
    in
    matcherFn current


completeLane : TilemapConfig -> CellIndex -> Node Connection -> Maybe (Edge Lane)
completeLane tilemapConfig spatialIndex current =
    let
        isPotentialConnection other =
            (other.id /= current.id)
                && hasSameDirection current other
                && isFacing current other

        checkCompatibility other =
            if other.label.kind == LaneConnector || other.label.kind == DeadendEntry then
                Just other

            else
                Nothing

        -- Find closest match by expanding in node's direction
        expandSearch currentCell =
            let
                nodesInCell =
                    Dict.get (Cell.coordinates currentCell) spatialIndex
                        |> Maybe.withDefault []

                compatibleNodes =
                    nodesInCell
                        |> List.filter isPotentialConnection
                        |> List.filterMap checkCompatibility
            in
            case compatibleNodes of
                [] ->
                    OrthogonalDirection.fromDirection2d current.label.direction
                        |> Maybe.andThen (\orthDir -> Cell.nextOrthogonalCell tilemapConfig orthDir currentCell)
                        |> Maybe.andThen expandSearch

                _ ->
                    compatibleNodes
                        |> List.sortBy (closestToOriginOrdering current)
                        |> List.head
    in
    expandSearch current.label.cell
        |> Maybe.map (connect current)


findLanesInsideCell : CellIndex -> Node Connection -> List (Edge Lane)
findLanesInsideCell spatialIndex current =
    let
        cellCoords =
            Cell.coordinates current.label.cell

        nodesInCell =
            Dict.get cellCoords spatialIndex
                |> Maybe.withDefault []
                |> List.Extra.uniqueBy .id
                |> List.filter (\node -> node.id /= current.id)

        laneConnectorsAmount =
            nodesInCell
                |> List.filter (\node -> node.label.kind == LaneConnector)
                |> List.length
    in
    List.filterMap
        (\other ->
            let
                isDeadendOrVariation =
                    laneConnectorsAmount == 1 && other.label.kind == LaneConnector
            in
            if isDeadendOrVariation || connectsWithinCell current other then
                Just (connect current other)

            else
                Nothing
        )
        nodesInCell


connect : Node Connection -> Node Connection -> Edge Lane
connect current match =
    Edge
        current.id
        match.id
        (Point2d.distanceFrom
            current.label.position
            match.label.position
        )


connectDeadendEntryWithExit : CellIndex -> Node Connection -> Maybe (Edge Lane)
connectDeadendEntryWithExit spatialIndex entry =
    let
        lanesDistance =
            outerLaneOffset |> Quantity.minus innerLaneOffset

        entryPosition =
            entry.label.position

        cellCoords =
            Cell.coordinates entry.label.cell

        nodesInSameCell =
            Dict.get cellCoords spatialIndex
                |> Maybe.withDefault []
                |> List.Extra.uniqueBy .id
    in
    nodesInSameCell
        |> List.filterMap
            (\other ->
                if other.label.kind == DeadendExit && Point2d.distanceFrom entryPosition other.label.position == lanesDistance then
                    Just
                        { from = entry.id
                        , to = other.id
                        , label = lanesDistance
                        }

                else
                    Nothing
            )
        |> List.head


hasSameDirection : Node Connection -> Node Connection -> Bool
hasSameDirection nodeA nodeB =
    connectionDirection nodeA == connectionDirection nodeB


isFacing : Node Connection -> Node Connection -> Bool
isFacing nodeA nodeB =
    let
        origin =
            connectionPosition nodeA

        target =
            connectionPosition nodeB

        direction =
            connectionDirection nodeA

        angleToTarget =
            origin |> Common.angleFromDirection direction target
    in
    angleToTarget == Quantity.zero


closestToOriginOrdering : Node Connection -> Node Connection -> Float
closestToOriginOrdering nodeA nodeB =
    let
        origin =
            connectionPosition nodeA

        target =
            connectionPosition nodeB
    in
    Point2d.distanceFrom origin target
        |> Length.inMeters


connectsWithinCell : Node Connection -> Node Connection -> Bool
connectsWithinCell current other =
    let
        fromDir =
            connectionDirection current

        toDir =
            connectionDirection other

        target =
            connectionPosition other

        ( leftLookupArea, rightLookupArea ) =
            connectionLookupArea current

        canContinueLeft =
            (toDir == fromDir || toDir == Direction2d.rotateCounterclockwise fromDir)
                && BoundingBox2d.contains target leftLookupArea

        canContinueRight =
            (toDir == fromDir || toDir == Direction2d.rotateClockwise fromDir)
                && BoundingBox2d.contains target rightLookupArea
    in
    canContinueLeft || canContinueRight


connectionLookupArea :
    Node Connection
    ->
        ( BoundingBox2d Length.Meters GlobalCoordinates
        , BoundingBox2d Length.Meters GlobalCoordinates
        )
connectionLookupArea node =
    let
        bb =
            Cell.boundingBox node.label.cell

        { lower, upper } =
            Common.splitBoundingBoxVertically bb

        { left, right } =
            Common.splitBoundingBoxHorizontally bb

        dir =
            connectionDirection node
    in
    if dir == Direction2d.positiveY then
        ( left, right )

    else if dir == Direction2d.positiveX then
        ( upper, lower )

    else if dir == Direction2d.negativeY then
        ( right, left )

    else if dir == Direction2d.negativeX then
        ( lower, upper )

    else
        ( left, right )



--
-- Traffic control
--


setupTrafficControl : TrafficLights -> RoadNetwork -> ( RoadNetwork, TrafficLights )
setupTrafficControl currentTrafficLights roadNetwork =
    Graph.fold
        (\nodeCtx ( roadNetworkAcc, trafficLightsAcc ) ->
            let
                ( nextNodeCtx, updatedTrafficLights ) =
                    resolveTrafficControl currentTrafficLights nodeCtx trafficLightsAcc roadNetworkAcc
            in
            ( Graph.insert nextNodeCtx roadNetworkAcc
            , updatedTrafficLights
            )
        )
        ( roadNetwork, Collection.empty )
        roadNetwork


resolveTrafficControl : TrafficLights -> RNNodeContext -> TrafficLights -> RoadNetwork -> ( RNNodeContext, TrafficLights )
resolveTrafficControl currentTrafficLights nodeCtx nextTrafficLights roadNetwork =
    let
        isLotExit =
            case nodeCtx.node.label.kind of
                LotExit _ ->
                    True

                _ ->
                    False
    in
    if isLotExit then
        ( addYield nodeCtx, nextTrafficLights )

    else
        case nodeCtx.node.label.environment of
            Intersection intersectionDirectionsAmount ->
                case intersectionDirectionsAmount of
                    -- Four-way intersection (or crossroads)
                    4 ->
                        case extractLotConnection nodeCtx roadNetwork of
                            Just ( _, dir ) ->
                                if dir == nodeCtx.node.label.direction then
                                    ( addYield nodeCtx, nextTrafficLights )

                                else
                                    ( setTrafficControl NoTrafficControl nodeCtx, nextTrafficLights )

                            Nothing ->
                                updateTrafficLight nodeCtx currentTrafficLights nextTrafficLights

                    -- 3 connections, a T-shaped intersection (though the type does not narrow the number down)
                    _ ->
                        let
                            nextNodeCtx =
                                if isOnPriorityRoad roadNetwork nodeCtx then
                                    setTrafficControl NoTrafficControl nodeCtx

                                else
                                    -- orphan road node
                                    addYield nodeCtx
                        in
                        ( nextNodeCtx, nextTrafficLights )

            Road ->
                ( setTrafficControl NoTrafficControl nodeCtx, nextTrafficLights )


addYield : RNNodeContext -> RNNodeContext
addYield nodeCtx =
    setTrafficControl
        (Yield
            (yieldCheckArea
                nodeCtx.node.label.position
                nodeCtx.node.label.direction
            )
        )
        nodeCtx


extractLotConnection : RNNodeContext -> RoadNetwork -> Maybe ( Id, Direction2d GlobalCoordinates )
extractLotConnection nodeCtx roadNetwork =
    List.foldl
        (\nodeId acc ->
            case Graph.get nodeId roadNetwork of
                Just outgoingNode ->
                    case outgoingNode.node.label.kind of
                        LotEntry id ->
                            Just ( id, outgoingNode.node.label.direction )

                        _ ->
                            acc

                Nothing ->
                    acc
        )
        Nothing
        (getOutgoingConnectionIds nodeCtx)


updateTrafficLight : RNNodeContext -> TrafficLights -> TrafficLights -> ( RNNodeContext, TrafficLights )
updateTrafficLight nodeCtx currentTrafficLights nextTrafficLights =
    let
        connection =
            nodeCtx.node.label

        ( trafficLight, updatedTrafficLights ) =
            case
                Collection.find
                    (\_ existingTrafficLight -> existingTrafficLight.position == connection.position)
                    currentTrafficLights
            of
                Just ( _, trafficLightMatch ) ->
                    ( trafficLightMatch
                    , Collection.addWithId trafficLightMatch.id trafficLightMatch nextTrafficLights
                    )

                Nothing ->
                    createTrafficLight connection nextTrafficLights
    in
    ( linkTrafficLightToNode trafficLight.id nodeCtx
    , updatedTrafficLights
    )


isOnPriorityRoad : RoadNetwork -> RNNodeContext -> Bool
isOnPriorityRoad roadNetwork nodeCtx =
    let
        otherNodeCtxs =
            getOutgoingConnectionIds nodeCtx
                |> List.filterMap (\nodeId -> Graph.get nodeId roadNetwork)
    in
    List.any
        (\other ->
            case other.node.label.kind of
                LotEntry _ ->
                    False

                _ ->
                    Direction2d.from
                        (connectionPosition nodeCtx.node)
                        (connectionPosition other.node)
                        |> Maybe.map (\dir -> Direction2d.xComponent dir == 0 || Direction2d.yComponent dir == 0)
                        |> Maybe.withDefault False
        )
        otherNodeCtxs


createTrafficLight : Connection -> TrafficLights -> ( TrafficLight, TrafficLights )
createTrafficLight connection trafficLights =
    let
        facing =
            Direction2d.reverse connection.direction

        builderFn =
            TrafficLight.new
                |> TrafficLight.withPosition connection.position
                |> TrafficLight.withFacing facing
                |> TrafficLight.build
    in
    Collection.addFromBuilder builderFn trafficLights


linkTrafficLightToNode : Id -> RNNodeContext -> RNNodeContext
linkTrafficLightToNode trafficLightId nodeCtx =
    setTrafficControl (Signal trafficLightId) nodeCtx


setTrafficControl : TrafficControl -> RNNodeContext -> RNNodeContext
setTrafficControl trafficControl nodeCtx =
    updateConnection (\connection -> { connection | trafficControl = trafficControl }) nodeCtx


updateConnection : (Connection -> Connection) -> RNNodeContext -> RNNodeContext
updateConnection updateFn nodeCtx =
    let
        node =
            nodeCtx.node

        label =
            nodeCtx.node.label

        nextLabel =
            updateFn label

        nextNode =
            { node | label = nextLabel }
    in
    { nodeCtx | node = nextNode }


yieldCheckArea :
    Point2d Length.Meters GlobalCoordinates
    -> Direction2d GlobalCoordinates
    -> BoundingBox2d Length.Meters GlobalCoordinates
yieldCheckArea yieldSignPosition centerOfYieldAreaDirection =
    let
        bbPoint1 =
            yieldSignPosition
                |> Point2d.translateIn
                    (Direction2d.rotateCounterclockwise centerOfYieldAreaDirection)
                    (Cell.size |> Quantity.twice)

        bbPoint2 =
            yieldSignPosition
                |> Point2d.translateIn centerOfYieldAreaDirection Cell.size
                |> Point2d.translateIn
                    (Direction2d.rotateClockwise centerOfYieldAreaDirection)
                    (Cell.size |> Quantity.twice)
    in
    BoundingBox2d.from bbPoint1 bbPoint2
