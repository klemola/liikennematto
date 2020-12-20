module RoadNetwork exposing (ConnectionKind(..), RoadNetwork, fromBoard, new)

import Board exposing (Board)
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
import Config exposing (innerLaneOffset, outerLaneOffset, tileSize)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Graph exposing (Edge, Graph, Node)
import Maybe.Extra as Maybe
import Position exposing (Position)
import Tile exposing (RoadKind(..), Tile(..))


type alias RoadNetwork =
    Graph Connection Lane


type alias Connection =
    { position : Position
    , direction : Direction
    , cell : Cell
    , kind : ConnectionKind
    }


type ConnectionKind
    = LaneStart
    | LaneEnd
    | DeadendEntry
    | DeadendExit
    | Stopgap


type alias Lane =
    ()


new : RoadNetwork
new =
    Graph.empty


fromBoard : Board -> RoadNetwork
fromBoard board =
    let
        tilePriority ( _, tile ) =
            case tile of
                TwoLaneRoad (Deadend _) _ ->
                    0

                Intersection _ _ ->
                    1

                _ ->
                    2

        nodes =
            createConnections
                { board = board
                , nodes = Dict.empty
                , remainingTiles =
                    Dict.toList board
                        |> List.sortBy tilePriority
                }
                |> Dict.values

        edges =
            createLanes nodes

        sizes =
            Debug.log "# nodes /edges " ( List.length nodes, List.length edges )
    in
    Graph.fromNodesAndEdges nodes edges



-- Connections


createConnections :
    { board : Board
    , nodes : Dict Position (Node Connection)
    , remainingTiles : List ( Cell, Tile )
    }
    -> Dict Position (Node Connection)
createConnections { nodes, board, remainingTiles } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                connectionsInTile =
                    toConnections board cell tile

                maybeCreateNode nodeSpec currentNodes =
                    if Dict.member nodeSpec.position currentNodes then
                        currentNodes

                    else
                        currentNodes
                            |> Dict.insert nodeSpec.position (Node (Dict.size currentNodes) nodeSpec)
            in
            createConnections
                { board = board
                , nodes =
                    connectionsInTile
                        |> List.foldl maybeCreateNode nodes
                , remainingTiles = otherTiles
                }


toConnections : Board -> Cell -> Tile -> List Connection
toConnections board cell tile =
    case tile of
        TwoLaneRoad (Regular _) _ ->
            []

        TwoLaneRoad (Deadend dir) _ ->
            connectionsByTileEntryDirection
                { board = board
                , cell = cell
                , tile = tile
                , baseConnectionKinds = ( DeadendExit, DeadendEntry )
                , direction = Direction.opposite dir
                }

        _ ->
            Tile.potentialConnections tile
                |> List.concatMap
                    (\dir ->
                        connectionsByTileEntryDirection
                            { board = board
                            , cell = cell
                            , tile = tile
                            , baseConnectionKinds = ( LaneStart, LaneEnd )
                            , direction = dir
                            }
                    )


connectionsByTileEntryDirection :
    { board : Board
    , cell : Cell
    , tile : Tile
    , baseConnectionKinds : ( ConnectionKind, ConnectionKind )
    , direction : Direction
    }
    -> List Connection
connectionsByTileEntryDirection { board, cell, tile, baseConnectionKinds, direction } =
    let
        ( originX, originY ) =
            Cell.bottomLeftCorner cell

        isStopgapTile =
            board
                |> Dict.get (Cell.next direction cell)
                |> Maybe.unwrap False (hasStopgapInbetween tile)

        ( startConnectionKind, endConnectionKind ) =
            if isStopgapTile then
                ( Stopgap, Stopgap )

            else
                baseConnectionKinds
    in
    case direction of
        Up ->
            [ { position = ( originX + outerLaneOffset, originY + tileSize )
              , direction = Up
              , cell = cell
              , kind = startConnectionKind
              }
            , { position = ( originX + innerLaneOffset, originY + tileSize )
              , direction = Down
              , cell = cell
              , kind = endConnectionKind
              }
            ]

        Right ->
            [ { position = ( originX + tileSize, originY + innerLaneOffset )
              , direction = Right
              , cell = cell
              , kind = startConnectionKind
              }
            , { position = ( originX + tileSize, originY + outerLaneOffset )
              , direction = Left
              , cell = cell
              , kind = endConnectionKind
              }
            ]

        Down ->
            [ { position = ( originX + innerLaneOffset, originY )
              , direction = Down
              , cell = cell
              , kind = startConnectionKind
              }
            , { position = ( originX + outerLaneOffset, originY )
              , direction = Up
              , cell = cell
              , kind = endConnectionKind
              }
            ]

        Left ->
            [ { position = ( originX, originY + outerLaneOffset )
              , direction = Left
              , cell = cell
              , kind = startConnectionKind
              }
            , { position = ( originX, originY + innerLaneOffset )
              , direction = Right
              , cell = cell
              , kind = endConnectionKind
              }
            ]


hasStopgapInbetween : Tile -> Tile -> Bool
hasStopgapInbetween tileA tileB =
    isCurveOrIntersection tileA && isCurveOrIntersection tileB


isCurveOrIntersection : Tile -> Bool
isCurveOrIntersection tile =
    case tile of
        TwoLaneRoad (Curve _) _ ->
            True

        Intersection _ _ ->
            True

        _ ->
            False



-- Lanes


connect : Node Connection -> Node Connection -> Edge Lane
connect current match =
    Edge current.id match.id ()


createLanes : List (Node Connection) -> List (Edge Lane)
createLanes nodes =
    List.concatMap (toEdges nodes) nodes


toEdges : List (Node Connection) -> Node Connection -> List (Edge Lane)
toEdges nodes current =
    case current.label.kind of
        LaneStart ->
            findLaneEnd nodes current
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        _ ->
            if startsEdgeInsideCell current then
                findLanesInsideCell nodes current

            else
                []


findLaneEnd : List (Node Connection) -> Node Connection -> Maybe (Edge Lane)
findLaneEnd nodes current =
    let
        isPotentialConnection other =
            hasSameDirection current other
                && isFacing current other

        checkCompatibility other =
            if other.label.kind == LaneEnd then
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
                if endsEdgeInsideCell other && connectsWithinCell current other then
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

        otherBB =
            nodeArea other

        leftBB =
            nodeConnectionRangeToLeft current (range + innerLaneOffset)

        rightBB =
            nodeConnectionRangeToRight current range

        canContinueLeft =
            (toDir == fromDir || toDir == Direction.previous fromDir) && Collision.aabb leftBB otherBB

        canContinueRight =
            (toDir == fromDir || toDir == Direction.next fromDir) && Collision.aabb rightBB otherBB
    in
    canContinueLeft || canContinueRight


nodeConnectionRangeToLeft : Node Connection -> Float -> BoundingBox
nodeConnectionRangeToLeft node range =
    let
        bb =
            nodeConnectionRangeToRight node range

        leftDir =
            Direction.previous (getDirection node)

        ( shiftedX, shiftedY ) =
            Position.shiftBy range ( bb.x, bb.y ) leftDir
    in
    { bb | x = shiftedX, y = shiftedY }


nodeConnectionRangeToRight : Node Connection -> Float -> BoundingBox
nodeConnectionRangeToRight node range =
    let
        ( nodeX, nodeY ) =
            getPosition node
    in
    case getDirection node of
        Up ->
            BoundingBox nodeX nodeY range tileSize

        Right ->
            BoundingBox nodeX (nodeY - range) tileSize range

        Down ->
            BoundingBox (nodeX - range) (nodeY - tileSize) range tileSize

        Left ->
            BoundingBox (nodeX - tileSize) nodeY tileSize range



-- Utility


getPosition : Node Connection -> Position
getPosition node =
    node.label.position


getDirection : Node Connection -> Direction
getDirection node =
    node.label.direction


startsEdgeInsideCell : Node Connection -> Bool
startsEdgeInsideCell node =
    node.label.kind == LaneEnd || node.label.kind == Stopgap


endsEdgeInsideCell : Node Connection -> Bool
endsEdgeInsideCell node =
    node.label.kind == LaneStart || node.label.kind == Stopgap


nodeArea : Node Connection -> BoundingBox
nodeArea node =
    let
        ( x, y ) =
            getPosition node
    in
    BoundingBox (x - 5) (y - 5) 10 10


hasSameDirection : Node Connection -> Node Connection -> Bool
hasSameDirection current other =
    getDirection current == getDirection other


isFacing : Node Connection -> Node Connection -> Bool
isFacing current other =
    let
        ( ( aX, aY ), ( bX, bY ) ) =
            ( getPosition current, getPosition other )
    in
    case getDirection current of
        Up ->
            aY < bY && aX == bX

        Right ->
            aX < bX && aY == bY

        Down ->
            aY > bY && aX == bX

        Left ->
            aX > bX && aY == bY


closestToOriginOrdering : Node Connection -> Node Connection -> Float
closestToOriginOrdering current other =
    let
        ( otherX, otherY ) =
            getPosition other
    in
    case getDirection current of
        Up ->
            otherY

        Right ->
            otherX

        Down ->
            0 - otherY

        Left ->
            0 - otherX
