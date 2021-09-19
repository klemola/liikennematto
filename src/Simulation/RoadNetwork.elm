module Simulation.RoadNetwork exposing
    ( findNodeByLotId
    , findNodeByNodeId
    , findNodeByPosition
    , findNodeReplacement
    , fromBoardAndLots
    , getOutgoingConnections
    , getRandomNode
    , lookupTree
    , setupTrafficControl
    )

import Angle
import BoundingBox2d
import Common
import Config exposing (pixelsToMeters, tileSizeInMeters)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction2d
import Graph exposing (Edge, Node, NodeId)
import IntDict
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Board as Board exposing (Board, Tile)
import Model.Cell as Cell exposing (Cell, OrthogonalDirection(..))
import Model.Entity as Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMDirection2d
        , LMEntityCoordinates
        , LMPoint2d
        )
import Model.Lot as Lot exposing (Lot, Lots)
import Model.RoadNetwork
    exposing
        ( Connection
        , ConnectionKind(..)
        , Lane
        , RNNodeContext
        , RoadNetwork
        , TrafficControl(..)
        )
import Model.TrafficLight as TrafficLight exposing (TrafficLight, TrafficLights)
import Point2d
import QuadTree
import Quantity
import Random
import Random.Extra
import Vector2d


type alias RNLookupTree =
    QuadTree.QuadTree Length.Meters LMEntityCoordinates LookupTreeEntry


type alias LookupTreeEntry =
    { id : Int, position : LMPoint2d, boundingBox : LMBoundingBox2d }


innerLaneOffset : Length
innerLaneOffset =
    pixelsToMeters 26


outerLaneOffset : Length
outerLaneOffset =
    pixelsToMeters 54


lookupTree : RoadNetwork -> RNLookupTree
lookupTree roadNetwork =
    QuadTree.init Board.boundingBox 4
        |> QuadTree.insertList
            (Graph.fold
                (\nodeCtx acc ->
                    { id = nodeCtx.node.id
                    , position = nodeCtx.node.label.position
                    , boundingBox = BoundingBox2d.singleton nodeCtx.node.label.position
                    }
                        :: acc
                )
                []
                roadNetwork
            )


fromBoardAndLots : Board -> Lots -> RoadNetwork
fromBoardAndLots board lots =
    let
        tilePriority ( _, tile ) =
            if Board.isDeadend tile then
                0

            else if Board.isIntersection tile then
                1

            else
                2

        nodes =
            createConnections
                { board = board
                , lots = lots
                , nodes = Dict.empty
                , remainingTiles =
                    Dict.toList board
                        |> List.sortBy tilePriority
                }
                |> Dict.values

        edges =
            createLanes nodes
    in
    Graph.fromNodesAndEdges nodes edges



--
-- Traffic control
--


setupTrafficControl : TrafficLights -> RoadNetwork -> ( RoadNetwork, TrafficLights )
setupTrafficControl currentTrafficLights roadNetwork =
    -- Room for improvement: merge this logic with "fromBoardAndLots"
    Graph.fold
        (updateNodeTrafficControl currentTrafficLights)
        ( roadNetwork, Dict.empty )
        roadNetwork


updateNodeTrafficControl : TrafficLights -> RNNodeContext -> ( RoadNetwork, TrafficLights ) -> ( RoadNetwork, TrafficLights )
updateNodeTrafficControl currentTrafficLights nodeCtx ( roadNetwork, nextTrafficLights ) =
    let
        connection =
            nodeCtx.node.label
    in
    case IntDict.size nodeCtx.outgoing of
        -- Four-way intersection (or crossroads)
        3 ->
            let
                trafficLight =
                    currentTrafficLights
                        |> Dict.find (\_ existingTrafficLight -> existingTrafficLight.position == connection.position)
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault (createTrafficLight connection nextTrafficLights)
            in
            ( Graph.insert (linkTrafficLightToNode trafficLight.id nodeCtx) roadNetwork
            , Dict.insert trafficLight.id trafficLight nextTrafficLights
            )

        -- Three-way intersection (or T-intersection)
        2 ->
            let
                nextNodeCtx =
                    if nodeCtx |> isOnPriorityRoad roadNetwork then
                        nodeCtx |> setTrafficControl None

                    else
                        -- orphan road node
                        nodeCtx |> setTrafficControl Yield
            in
            ( Graph.insert nextNodeCtx roadNetwork
            , nextTrafficLights
            )

        -- Not an intersection (assuming max four ways)
        _ ->
            ( Graph.insert (nodeCtx |> setTrafficControl None) roadNetwork
            , nextTrafficLights
            )


createTrafficLight : Connection -> TrafficLights -> TrafficLight
createTrafficLight connection nextTrafficLights =
    let
        nextId =
            Entity.nextId nextTrafficLights

        facing =
            Direction2d.reverse connection.direction

        color =
            if connection.direction == Direction2d.positiveX || connection.direction == Direction2d.negativeX then
                TrafficLight.Green

            else
                TrafficLight.Red
    in
    TrafficLight.new
        |> TrafficLight.withPosition connection.position
        |> TrafficLight.withFacing facing
        |> TrafficLight.withColor color
        |> TrafficLight.build nextId



-- Connections


type alias NodesMemo =
    -- Index nodes by their position (x, y) to avoid duplicates
    Dict ( Float, Float ) (Node Connection)


createConnections :
    { board : Board
    , lots : Dict Int Lot
    , nodes : NodesMemo
    , remainingTiles : List ( Cell, Tile )
    }
    -> NodesMemo
createConnections { nodes, board, remainingTiles, lots } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                connectionsInTile =
                    toConnections board cell tile lots

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
                { board = board
                , lots = lots
                , nodes =
                    connectionsInTile
                        |> List.foldl maybeCreateNode nodes
                , remainingTiles = otherTiles
                }


toConnections : Board -> Cell -> Tile -> Lots -> List Connection
toConnections board cell tile lots =
    if tile == Board.twoLaneRoadHorizontal then
        lotConnections cell tile Cell.right lots

    else if tile == Board.twoLaneRoadVertical then
        lotConnections cell tile Cell.up lots

    else if Board.isDeadend tile then
        Board.potentialConnections tile
            |> List.concatMap (Cell.orthogonalDirectionToLmDirection >> deadendConnections cell tile)

    else
        Board.potentialConnections tile
            |> List.concatMap (connectionsByTileEntryDirection board cell tile)


lotConnections : Cell -> Tile -> LMDirection2d -> Lots -> List Connection
lotConnections cell tile trafficDirection lots =
    case Dict.find (\_ lot -> Lot.anchorCell lot == cell) lots of
        Just ( id, lot ) ->
            let
                ( posA, posB ) =
                    laneCenterPositionsByDirection cell trafficDirection

                anchorDirection =
                    Tuple.second lot.anchor
                        |> Cell.orthogonalDirectionToLmDirection

                ( position, direction ) =
                    if anchorDirection == Direction2d.rotateClockwise trafficDirection then
                        ( posA, trafficDirection )

                    else
                        ( posB, Direction2d.reverse trafficDirection )
            in
            [ { kind = LotEntry
              , position = position
              , direction = direction
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Just id
              }
            ]

        Nothing ->
            []


deadendConnections : Cell -> Tile -> LMDirection2d -> List Connection
deadendConnections cell tile trafficDirection =
    let
        ( entryPosition, exitPosition ) =
            laneCenterPositionsByDirection cell trafficDirection

        entryConnection =
            { kind = DeadendEntry
            , position = entryPosition
            , direction = trafficDirection
            , cell = cell
            , tile = tile
            , trafficControl = None
            , lotId = Nothing
            }

        exitConnection =
            { kind = DeadendExit
            , position = exitPosition
            , direction = Direction2d.reverse trafficDirection
            , cell = cell
            , tile = tile
            , trafficControl = None
            , lotId = Nothing
            }
    in
    [ entryConnection
    , exitConnection
    ]


laneCenterPositionsByDirection : Cell -> LMDirection2d -> ( LMPoint2d, LMPoint2d )
laneCenterPositionsByDirection cell trafficDirection =
    let
        connectionOffsetFromTileCenter =
            tileSizeInMeters
                |> Quantity.half
                |> Quantity.minus innerLaneOffset

        tileCenterPosition =
            Cell.center cell
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


connectionsByTileEntryDirection : Board -> Cell -> Tile -> OrthogonalDirection -> List Connection
connectionsByTileEntryDirection board cell tile direction =
    let
        origin =
            Cell.bottomLeftCorner cell

        isStopgapTile =
            board
                |> Dict.get (Cell.next direction cell)
                |> Maybe.unwrap False (hasStopgapInbetween tile)

        ( startConnectionKind, endConnectionKind ) =
            if isStopgapTile then
                ( Stopgap, Stopgap )

            else
                ( LaneStart, LaneEnd )

        shift x y =
            origin |> Point2d.translateBy (Vector2d.xy x y)
    in
    case direction of
        Up ->
            [ { kind = startConnectionKind
              , position = shift outerLaneOffset tileSizeInMeters
              , direction = Cell.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift innerLaneOffset tileSizeInMeters
              , direction = Cell.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Right ->
            [ { kind = startConnectionKind
              , position = shift tileSizeInMeters innerLaneOffset
              , direction = Cell.right
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift tileSizeInMeters outerLaneOffset
              , direction = Cell.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Down ->
            [ { kind = startConnectionKind
              , position = shift innerLaneOffset Quantity.zero
              , direction = Cell.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift outerLaneOffset Quantity.zero
              , direction = Cell.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Left ->
            [ { kind = startConnectionKind
              , position = shift Quantity.zero outerLaneOffset
              , direction = Cell.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift Quantity.zero innerLaneOffset
              , direction = Cell.right
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]


hasStopgapInbetween : Tile -> Tile -> Bool
hasStopgapInbetween tileA tileB =
    isCurveOrIntersection tileA && isCurveOrIntersection tileB


isCurveOrIntersection : Tile -> Bool
isCurveOrIntersection tile =
    Board.isCurve tile || Board.isIntersection tile



-- Lanes


connect : Node Connection -> Node Connection -> Edge Lane
connect current match =
    Edge current.id match.id ()


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
                LaneStart ->
                    findLaneEnd nodes >> potentialResultToEdges

                LaneEnd ->
                    findLanesInsideCell nodes

                DeadendEntry ->
                    connectDeadendEntryWithExit >> potentialResultToEdges

                DeadendExit ->
                    findLaneEnd nodes >> potentialResultToEdges

                LotEntry ->
                    findLaneEnd nodes >> potentialResultToEdges

                Stopgap ->
                    findLanesInsideCell nodes
    in
    matcherFn current


findLaneEnd : List (Node Connection) -> Node Connection -> Maybe (Edge Lane)
findLaneEnd nodes current =
    let
        isPotentialConnection other =
            (other.id /= current.id)
                && hasSameDirection current other
                && isFacing current other

        checkCompatibility other =
            if other.label.kind == LaneEnd || other.label.kind == DeadendEntry || other.label.kind == LotEntry then
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


findLanesInsideCell : List (Node Connection) -> Node Connection -> List (Edge Lane)
findLanesInsideCell nodes current =
    nodes
        |> List.filterMap
            (\other ->
                if current.id /= other.id && endsEdgeInsideCell other && connectsWithinCell current other then
                    Just (connect current other)

                else
                    Nothing
            )


connectsWithinCell : Node Connection -> Node Connection -> Bool
connectsWithinCell current other =
    let
        ( fromDir, toDir ) =
            ( getDirection current, getDirection other )

        range =
            Quantity.half tileSizeInMeters

        target =
            getPosition other

        leftLookupArea =
            connectionLookupAreaToLeft current (range |> Quantity.plus innerLaneOffset)

        rightLookupArea =
            connectionLookupAreaToRight current range

        canContinueLeft =
            (toDir == fromDir || toDir == Direction2d.rotateCounterclockwise fromDir) && BoundingBox2d.contains target leftLookupArea

        canContinueRight =
            (toDir == fromDir || toDir == Direction2d.rotateClockwise fromDir) && BoundingBox2d.contains target rightLookupArea
    in
    canContinueLeft || canContinueRight


connectionLookupAreaToLeft : Node Connection -> Length -> LMBoundingBox2d
connectionLookupAreaToLeft node range =
    let
        bb =
            connectionLookupAreaToRight node range

        leftDir =
            Direction2d.rotateCounterclockwise (getDirection node)
    in
    BoundingBox2d.translateIn leftDir range bb


connectionLookupAreaToRight : Node Connection -> Length -> LMBoundingBox2d
connectionLookupAreaToRight node range =
    let
        origin =
            getPosition node

        nodeDirection =
            getDirection node

        nodeDirectionRotatedRight =
            Direction2d.rotateClockwise nodeDirection

        otherCorner =
            origin
                |> Point2d.translateIn nodeDirection tileSizeInMeters
                |> Point2d.translateIn nodeDirectionRotatedRight range
    in
    BoundingBox2d.from origin otherCorner


connectDeadendEntryWithExit : Node Connection -> Maybe (Edge Lane)
connectDeadendEntryWithExit entry =
    -- an assumption about node creation order (implied ID) is a cheap way to create the edge
    -- Room for improvement: really try to find a node that is at the expected Position
    Just { from = entry.id, to = entry.id + 1, label = () }



-- Utility


getPosition : Node Connection -> LMPoint2d
getPosition node =
    node.label.position


getDirection : Node Connection -> LMDirection2d
getDirection node =
    node.label.direction


endsEdgeInsideCell : Node Connection -> Bool
endsEdgeInsideCell node =
    node.label.kind == LaneStart || node.label.kind == Stopgap || node.label.kind == DeadendExit


hasSameDirection : Node Connection -> Node Connection -> Bool
hasSameDirection nodeA nodeB =
    getDirection nodeA == getDirection nodeB


isFacing : Node Connection -> Node Connection -> Bool
isFacing nodeA nodeB =
    let
        origin =
            getPosition nodeA

        target =
            getPosition nodeB

        direction =
            getDirection nodeA

        angleToTarget =
            origin |> Common.angleFromDirection direction target
    in
    Angle.inDegrees angleToTarget == 0


isOnPriorityRoad : RoadNetwork -> RNNodeContext -> Bool
isOnPriorityRoad roadNetwork nodeCtx =
    let
        otherNodeCtxs =
            getOutgoingConnections nodeCtx
                |> List.filterMap (\nodeId -> Graph.get nodeId roadNetwork)
    in
    List.any (.node >> isParallel nodeCtx.node) otherNodeCtxs


isParallel : Node Connection -> Node Connection -> Bool
isParallel nodeA nodeB =
    Direction2d.from
        (getPosition nodeA)
        (getPosition nodeB)
        |> Maybe.map (\dir -> Direction2d.xComponent dir == 0 || Direction2d.yComponent dir == 0)
        |> Maybe.withDefault False


closestToOriginOrdering : Node Connection -> Node Connection -> Float
closestToOriginOrdering nodeA nodeB =
    let
        origin =
            getPosition nodeA

        target =
            getPosition nodeB
    in
    Point2d.distanceFrom origin target
        |> Length.inMeters


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



-- Queries


findNodeByLotId : RoadNetwork -> Int -> Maybe RNNodeContext
findNodeByLotId roadNetwork lotId =
    roadNetwork
        |> Graph.fold
            (\ctx acc ->
                if ctx.node.label.lotId == Just lotId then
                    Just ctx

                else
                    acc
            )
            Nothing


findNodeByNodeId : RoadNetwork -> NodeId -> Maybe RNNodeContext
findNodeByNodeId roadNetwork nodeId =
    Graph.get nodeId roadNetwork


findNodeByPosition : RoadNetwork -> LMPoint2d -> Maybe RNNodeContext
findNodeByPosition roadNetwork position =
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


getRandomNode : RoadNetwork -> Random.Seed -> ( Maybe RNNodeContext, Random.Seed )
getRandomNode roadNetwork seed =
    let
        randomNodeGenerator =
            roadNetwork
                |> Graph.nodeIds
                |> Random.Extra.sample
                |> Random.map (Maybe.andThen (findNodeByNodeId roadNetwork))
    in
    Random.step randomNodeGenerator seed


getOutgoingConnections : RNNodeContext -> List NodeId
getOutgoingConnections nodeCtx =
    Graph.alongOutgoingEdges nodeCtx


findNodeReplacement : RNLookupTree -> RNNodeContext -> Maybe Int
findNodeReplacement nodeLookup target =
    -- Tries to find a node in the lookup tree that could replace the reference node.
    -- Useful in maintaning route after the road network has been updated.
    let
        targetPosition =
            target.node.label.position

        positionMatches =
            nodeLookup
                |> QuadTree.findIntersecting
                    { id = target.node.id
                    , position = targetPosition
                    , boundingBox = BoundingBox2d.singleton targetPosition
                    }
    in
    case positionMatches of
        match :: _ ->
            Just match.id

        [] ->
            Nothing
