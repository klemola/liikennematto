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
    , outgoingConnectionsAmount
    , size
    )

import BoundingBox2d
import Collection exposing (Collection, Id, idMatches)
import Common
import Data.Assets exposing (innerLaneOffset, outerLaneOffset)
import Data.Lots exposing (drivewayOffset)
import Dict exposing (Dict)
import Direction2d
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import IntDict
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Geometry as Geometry
    exposing
        ( LMBoundingBox2d
        , LMDirection2d
        , LMPoint2d
        , OrthogonalDirection(..)
        , oppositeOrthogonalDirection
        , orthogonalDirectionToLmDirection
        )
import Point2d
import Quantity
import Random
import Random.Extra
import Simulation.TrafficLight as TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( TileListFilter(..)
        , Tilemap
        , anchorByCell
        , fixedTileByCell
        , getTilemapConfig
        , tilemapToList
        )
import Tilemap.Tile
    exposing
        ( Tile
        , isBasicRoad
        , isCurve
        , isDeadend
        , isIntersection
        , isLotEntry
        , potentialConnections
        )
import Vector2d exposing (Vector2d)


type alias RoadNetwork =
    Graph Connection Lane


type alias RNNodeContext =
    NodeContext Connection Lane


type alias RNNode =
    Graph.Node Connection


type alias Connection =
    { kind : ConnectionKind
    , position : LMPoint2d
    , direction : LMDirection2d
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
    | Intersection


type TrafficControl
    = Signal Id
    | Yield LMBoundingBox2d
    | NoTrafficControl


type alias Lane =
    Length


empty : RoadNetwork
empty =
    Graph.empty


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


nodeByPosition : RoadNetwork -> LMPoint2d -> Maybe RNNodeContext
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


getRandomNode : RoadNetwork -> Random.Seed -> (RNNode -> Bool) -> Maybe RNNodeContext
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
        |> Tuple.first


outgoingConnectionsAmount : RNNodeContext -> Int
outgoingConnectionsAmount nodeCtx =
    IntDict.size nodeCtx.outgoing


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


nodePosition : RNNodeContext -> LMPoint2d
nodePosition nodeCtx =
    nodeCtx.node.label.position


nodeDirection : RNNodeContext -> LMDirection2d
nodeDirection nodeCtx =
    nodeCtx.node.label.direction


connectionPosition : Node Connection -> LMPoint2d
connectionPosition node =
    node.label.position


connectionDirection : Node Connection -> LMDirection2d
connectionDirection node =
    node.label.direction


buildRoadNetwork : Tilemap -> Collection TrafficLight -> ( RoadNetwork, Collection TrafficLight )
buildRoadNetwork tilemap trafficLights =
    let
        tilePriority ( _, tile ) =
            if isDeadend tile then
                0

            else if isLotEntry tile then
                1

            else if isIntersection tile then
                2

            else
                3

        nodes =
            createConnections
                { tilemap = tilemap
                , nodes = Dict.empty
                , remainingTiles =
                    tilemap
                        |> tilemapToList Tuple.pair NoFilter
                        |> List.sortBy tilePriority
                }
                |> Dict.values

        edges =
            createLanes nodes

        roadNetwork =
            Graph.fromNodesAndEdges nodes edges
    in
    roadNetwork |> setupTrafficControl trafficLights


setupTrafficControl : Collection TrafficLight -> RoadNetwork -> ( RoadNetwork, Collection TrafficLight )
setupTrafficControl currentTrafficLights roadNetwork =
    Graph.fold
        (updateNodeTrafficControl currentTrafficLights)
        ( roadNetwork, Collection.empty )
        roadNetwork



--
-- Connections
--


type alias NodesMemo =
    -- Index nodes by their position (x, y) to avoid duplicates
    Dict ( Float, Float ) (Node Connection)


createConnections :
    { tilemap : Tilemap
    , nodes : NodesMemo
    , remainingTiles : List ( Cell, Tile )
    }
    -> NodesMemo
createConnections { nodes, tilemap, remainingTiles } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                connectionsInTile =
                    toConnections tilemap cell tile

                maybeCreateNode nodeSpec currentNodes =
                    let
                        key =
                            Point2d.toTuple Length.inMeters nodeSpec.position
                    in
                    if Dict.member key currentNodes then
                        currentNodes

                    else
                        currentNodes
                            |> Dict.insert key (Node (Dict.size currentNodes) nodeSpec)
            in
            createConnections
                { tilemap = tilemap
                , nodes =
                    connectionsInTile
                        |> List.foldl maybeCreateNode nodes
                , remainingTiles = otherTiles
                }


toConnections : Tilemap -> Cell -> Tile -> List Connection
toConnections tilemap cell tile =
    if isBasicRoad tile then
        []

    else if isDeadend tile then
        potentialConnections tile
            |> List.concatMap
                (oppositeOrthogonalDirection
                    >> orthogonalDirectionToLmDirection
                    >> deadendConnections cell
                )

    else
        potentialConnections tile
            |> List.concatMap (connectionsByTileEntryDirection tilemap cell tile)


deadendConnections : Cell -> LMDirection2d -> List Connection
deadendConnections cell trafficDirection =
    let
        ( entryPosition, exitPosition ) =
            laneCenterPositionsByDirection cell trafficDirection

        entryConnection =
            { kind = DeadendEntry
            , position = entryPosition
            , direction = trafficDirection
            , cell = cell
            , trafficControl = NoTrafficControl
            }

        exitConnection =
            { kind = DeadendExit
            , position = exitPosition
            , direction = Direction2d.reverse trafficDirection
            , cell = cell
            , trafficControl = NoTrafficControl
            }
    in
    [ entryConnection
    , exitConnection
    ]


laneCenterPositionsByDirection : Cell -> LMDirection2d -> ( LMPoint2d, LMPoint2d )
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


connectionsByTileEntryDirection : Tilemap -> Cell -> Tile -> OrthogonalDirection -> List Connection
connectionsByTileEntryDirection tilemap cell tile direction =
    let
        origin =
            Cell.bottomLeftCorner cell

        anchor =
            anchorByCell tilemap cell

        startDirection =
            Geometry.orthogonalDirectionToLmDirection direction

        endDirection =
            Direction2d.reverse startDirection

        ( startConnectionKind, endConnectionKind, extraOffset ) =
            case anchor of
                Just ( lotId, anchorDirection ) ->
                    if direction == anchorDirection then
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
      , direction = startDirection
      , cell = startConnectionCell
      , trafficControl = NoTrafficControl
      }
    , { kind = endConnectionKind
      , position = origin |> Point2d.translateBy (endOffset |> Vector2d.plus extraOffset)
      , direction = endDirection
      , cell = cell
      , trafficControl = NoTrafficControl
      }
    ]


toLotOffset : OrthogonalDirection -> Vector2d Length.Meters coordinates
toLotOffset anchorDirection =
    case anchorDirection of
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
                    -- Some tile combinations have overlapping connections on their edges.
                    -- Using the neighbor tile's cell as the basis for the connection ensures all lanes are mapped correctly.
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
    isCurve tile || isIntersection tile || isLotEntry tile



--
-- Lanes
--


connect : Node Connection -> Node Connection -> Edge Lane
connect current match =
    Edge
        current.id
        match.id
        (Point2d.distanceFrom
            current.label.position
            match.label.position
        )


createLanes : List (Node Connection) -> List (Edge Lane)
createLanes nodes =
    List.concatMap (toEdges nodes) nodes


toEdges : List (Node Connection) -> Node Connection -> List (Edge Lane)
toEdges nodes current =
    let
        potentialResultToEdges =
            Maybe.unwrap [] List.singleton

        matcherFn =
            case current.label.kind of
                DeadendEntry ->
                    connectDeadendEntryWithExit >> potentialResultToEdges

                DeadendExit ->
                    completeLane nodes >> potentialResultToEdges

                LotEntry _ ->
                    always []

                LotExit _ ->
                    findLanesInsideCell nodes

                _ ->
                    if Cell.centerPoint current.label.cell |> Common.isInTheNormalPlaneOf current.label.direction current.label.position then
                        findLanesInsideCell nodes

                    else
                        -- the node is facing away from its' Cell
                        completeLane nodes >> potentialResultToEdges
    in
    matcherFn current


completeLane : List (Node Connection) -> Node Connection -> Maybe (Edge Lane)
completeLane nodes current =
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
    in
    nodes
        |> List.filter isPotentialConnection
        |> List.sortBy (closestToOriginOrdering current)
        |> List.head
        |> Maybe.andThen checkCompatibility
        |> Maybe.map (connect current)


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


findLanesInsideCell : List (Node Connection) -> Node Connection -> List (Edge Lane)
findLanesInsideCell nodes current =
    nodes
        |> List.filterMap
            (\other ->
                if current.id /= other.id && connectsWithinCell current other then
                    Just (connect current other)

                else
                    Nothing
            )


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


connectionLookupArea : Node Connection -> ( LMBoundingBox2d, LMBoundingBox2d )
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
    if dir == Geometry.up then
        ( left, right )

    else if dir == Geometry.right then
        ( upper, lower )

    else if dir == Geometry.down then
        ( right, left )

    else if dir == Geometry.left then
        ( lower, upper )

    else
        ( left, right )


connectDeadendEntryWithExit : Node Connection -> Maybe (Edge Lane)
connectDeadendEntryWithExit entry =
    -- an assumption about node creation order (implied ID) is a cheap way to create the edge
    -- Room for improvement: really try to find a node that is at the expected Position
    Just { from = entry.id, to = entry.id + 1, label = Cell.size |> Quantity.half }



--
-- Traffic control
--


updateNodeTrafficControl : Collection TrafficLight -> RNNodeContext -> ( RoadNetwork, Collection TrafficLight ) -> ( RoadNetwork, Collection TrafficLight )
updateNodeTrafficControl currentTrafficLights nodeCtx ( roadNetwork, trafficLights ) =
    case outgoingConnectionsAmount nodeCtx of
        -- Four-way intersection (or crossroads)
        3 ->
            let
                connection =
                    nodeCtx.node.label

                ( trafficLight, nextTrafficLights ) =
                    case Collection.find (\_ existingTrafficLight -> existingTrafficLight.position == connection.position) currentTrafficLights of
                        Just ( _, trafficLightMatch ) ->
                            ( trafficLightMatch
                            , Collection.addWithId trafficLightMatch.id trafficLightMatch trafficLights
                            )

                        Nothing ->
                            createTrafficLight connection trafficLights
            in
            ( Graph.insert (linkTrafficLightToNode trafficLight.id nodeCtx) roadNetwork
            , nextTrafficLights
            )

        -- Three-way intersection (or T-intersection)
        2 ->
            let
                nextNodeCtx =
                    if nodeCtx |> isOnPriorityRoad roadNetwork then
                        nodeCtx |> setTrafficControl NoTrafficControl

                    else
                        -- orphan road node
                        nodeCtx
                            |> setTrafficControl
                                (Yield
                                    (yieldCheckArea
                                        nodeCtx.node.label.position
                                        nodeCtx.node.label.direction
                                    )
                                )
            in
            ( Graph.insert nextNodeCtx roadNetwork
            , trafficLights
            )

        -- Not an intersection (assuming max four ways)
        _ ->
            ( Graph.insert (nodeCtx |> setTrafficControl NoTrafficControl) roadNetwork
            , trafficLights
            )


isOnPriorityRoad : RoadNetwork -> RNNodeContext -> Bool
isOnPriorityRoad roadNetwork nodeCtx =
    let
        otherNodeCtxs =
            getOutgoingConnectionIds nodeCtx
                |> List.filterMap (\nodeId -> Graph.get nodeId roadNetwork)
    in
    List.any (.node >> isParallel nodeCtx.node) otherNodeCtxs


isParallel : Node Connection -> Node Connection -> Bool
isParallel nodeA nodeB =
    Direction2d.from
        (connectionPosition nodeA)
        (connectionPosition nodeB)
        |> Maybe.map (\dir -> Direction2d.xComponent dir == 0 || Direction2d.yComponent dir == 0)
        |> Maybe.withDefault False


createTrafficLight : Connection -> Collection TrafficLight -> ( TrafficLight, Collection TrafficLight )
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
    let
        node =
            nodeCtx.node

        label =
            nodeCtx.node.label

        nextLabel =
            { label | trafficControl = trafficControl }

        nextNode =
            { node | label = nextLabel }
    in
    { nodeCtx | node = nextNode }


yieldCheckArea : LMPoint2d -> LMDirection2d -> LMBoundingBox2d
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
