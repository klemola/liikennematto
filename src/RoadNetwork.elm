module RoadNetwork exposing
    ( Connection
    , ConnectionKind(..)
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , findNodeByLotId
    , findNodeByNodeId
    , findNodeByPosition
    , fromBoardAndLots
    , getOutgoingConnections
    , getRandomNode
    , new
    , setupTrafficLights
    , toDotString
    )

import Angle
import Board exposing (Board, Tile, crossIntersection)
import BoundingBox2d
import Cell exposing (Cell, OrthogonalDirection(..))
import Config exposing (innerLaneOffset, outerLaneOffset, tileSize)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction2d
import Entity exposing (Id)
import Geometry exposing (LMBoundingBox2d, LMDirection2d, LMPoint2d)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Graph.DOT
import Lot exposing (Lot, Lots)
import Maybe.Extra as Maybe
import Point2d
import Random
import Random.Extra
import TrafficLight exposing (TrafficLight, TrafficLights)


type alias RoadNetwork =
    Graph Connection Lane


type alias RNNodeContext =
    NodeContext Connection Lane


type alias Connection =
    { kind : ConnectionKind
    , position : LMPoint2d
    , direction : LMDirection2d
    , cell : Cell
    , tile : Tile

    -- Room for improvement: support more than one lot per (anchor) cell
    , trafficControl : TrafficControl
    , lotId : Maybe Int
    }


type ConnectionKind
    = LaneStart
    | LaneEnd
    | DeadendEntry
    | DeadendExit
    | LotEntry
    | Stopgap


type TrafficControl
    = Signal Id
    | None


type alias Lane =
    ()


new : RoadNetwork
new =
    Graph.empty


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
-- Traffic lights
--


setupTrafficLights : TrafficLights -> RoadNetwork -> ( RoadNetwork, TrafficLights )
setupTrafficLights currentTrafficLights roadNetwork =
    roadNetwork
        |> Graph.fold (toTrafficLights currentTrafficLights) ( roadNetwork, Dict.empty )


toTrafficLights : TrafficLights -> RNNodeContext -> ( RoadNetwork, TrafficLights ) -> ( RoadNetwork, TrafficLights )
toTrafficLights currentTrafficLights nodeCtx ( roadNetwork, nextTrafficLights ) =
    -- keeps existing traffic lights if possible, may create new ones
    let
        connection =
            nodeCtx.node.label
    in
    if connection.kind == LaneEnd && connection.tile == crossIntersection then
        let
            trafficLight =
                currentTrafficLights
                    |> Dict.find (\_ existingTrafficLight -> existingTrafficLight.position == connection.position)
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault (createTrafficLight connection nextTrafficLights)
        in
        ( roadNetwork
            |> Graph.insert (linkTrafficLightToNode trafficLight.id nodeCtx)
        , nextTrafficLights
            |> Dict.insert trafficLight.id trafficLight
        )

    else
        ( roadNetwork
            |> Graph.update nodeCtx.node.id (Maybe.map removeTrafficLightLinkFromNode)
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
                            Geometry.pointToPositionAsTuple nodeSpec.position
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
        halfTile =
            tileSize / 2

        connectionOffsetFromTileCenter =
            Geometry.toLMUnits <| halfTile - innerLaneOffset

        tileCenterPosition =
            Cell.center cell
    in
    ( tileCenterPosition
        |> Geometry.translatePointIn (Direction2d.rotateClockwise trafficDirection) connectionOffsetFromTileCenter
    , tileCenterPosition
        |> Geometry.translatePointIn (Direction2d.rotateCounterclockwise trafficDirection) connectionOffsetFromTileCenter
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
            origin
                |> Geometry.translatePointBy x y
    in
    case direction of
        Up ->
            [ { kind = startConnectionKind
              , position = shift outerLaneOffset tileSize
              , direction = Cell.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift innerLaneOffset tileSize
              , direction = Cell.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Right ->
            [ { kind = startConnectionKind
              , position = shift tileSize innerLaneOffset
              , direction = Cell.right
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift tileSize outerLaneOffset
              , direction = Cell.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Down ->
            [ { kind = startConnectionKind
              , position = shift innerLaneOffset 0
              , direction = Cell.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift outerLaneOffset 0
              , direction = Cell.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Left ->
            [ { kind = startConnectionKind
              , position = shift 0 outerLaneOffset
              , direction = Cell.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift 0 innerLaneOffset
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
            tileSize / 2

        target =
            getPosition other

        leftLookupArea =
            connectionLookupAreaToLeft current (range + innerLaneOffset)

        rightLookupArea =
            connectionLookupAreaToRight current range

        canContinueLeft =
            (toDir == fromDir || toDir == Direction2d.rotateCounterclockwise fromDir) && BoundingBox2d.contains target leftLookupArea

        canContinueRight =
            (toDir == fromDir || toDir == Direction2d.rotateClockwise fromDir) && BoundingBox2d.contains target rightLookupArea
    in
    canContinueLeft || canContinueRight


connectionLookupAreaToLeft : Node Connection -> Float -> LMBoundingBox2d
connectionLookupAreaToLeft node range =
    let
        bb =
            connectionLookupAreaToRight node range

        leftDir =
            Direction2d.rotateCounterclockwise (getDirection node)
    in
    Geometry.translateBoundingBoxIn leftDir range bb


connectionLookupAreaToRight : Node Connection -> Float -> LMBoundingBox2d
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
                |> Geometry.translatePointIn nodeDirection (Geometry.toLMUnits tileSize)
                |> Geometry.translatePointIn nodeDirectionRotatedRight (Geometry.toLMUnits range)
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
hasSameDirection current other =
    getDirection current == getDirection other


isFacing : Node Connection -> Node Connection -> Bool
isFacing current other =
    let
        origin =
            getPosition current

        target =
            getPosition other

        direction =
            getDirection current

        angleToTarget =
            origin
                |> Geometry.angleFromDirection direction target
    in
    Angle.inDegrees angleToTarget == 0


closestToOriginOrdering : Node Connection -> Node Connection -> Float
closestToOriginOrdering current other =
    let
        origin =
            getPosition current

        target =
            getPosition other
    in
    Point2d.distanceFrom origin target
        |> Geometry.toFloat


linkTrafficLightToNode : Id -> RNNodeContext -> RNNodeContext
linkTrafficLightToNode trafficLightId nodeCtx =
    setTrafficControl (Signal trafficLightId) nodeCtx


removeTrafficLightLinkFromNode : RNNodeContext -> RNNodeContext
removeTrafficLightLinkFromNode nodeCtx =
    setTrafficControl None nodeCtx


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


getRandomNode : RoadNetwork -> Random.Seed -> Maybe RNNodeContext
getRandomNode roadNetwork seed =
    let
        randomNodeIdGenerator =
            roadNetwork
                |> Graph.nodeIds
                |> Random.Extra.sample
    in
    Random.step randomNodeIdGenerator seed
        |> Tuple.first
        |> Maybe.andThen (findNodeByNodeId roadNetwork)


getOutgoingConnections : RNNodeContext -> List NodeId
getOutgoingConnections nodeCtx =
    Graph.alongOutgoingEdges nodeCtx



-- Debug


connectionKindToString : ConnectionKind -> String
connectionKindToString kind =
    case kind of
        LaneStart ->
            "Lane start"

        LaneEnd ->
            "Lane end"

        DeadendEntry ->
            "Deadend entry"

        DeadendExit ->
            "Deadend exit"

        LotEntry ->
            "Lot entry"

        Stopgap ->
            "Stopgap"


toDotString : RoadNetwork -> String
toDotString =
    let
        nodeFormatter =
            \connection -> Just (connectionKindToString connection.kind)

        edgeFormatter =
            \_ -> Nothing
    in
    Graph.DOT.output nodeFormatter edgeFormatter
