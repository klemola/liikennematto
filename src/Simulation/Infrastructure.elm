module Simulation.Infrastructure exposing
    ( buildRoadAt
    , canBuildRoadAt
    , connectLotToRoadNetwork
    , findNodeReplacement
    , removeRoadAt
    )

import BoundingBox2d
import Common
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction2d
import Graph exposing (Edge, Node)
import IntDict
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Entity as Entity exposing (Id)
import Model.Geometry as Geometry
    exposing
        ( LMBoundingBox2d
        , LMDirection2d
        , LMPoint2d
        , pixelsToMeters
        )
import Model.Lookup exposing (roadNetworkLookup)
import Model.Lot as Lot exposing (Lot, Lots)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( Connection
        , ConnectionKind(..)
        , Lane
        , RNNodeContext
        , RoadNetwork
        , TrafficControl(..)
        )
import Model.Tilemap as Tilemap
    exposing
        ( Cell
        , OrthogonalDirection(..)
        , Tile
        , Tilemap
        , tileSize
        )
import Model.TrafficLight as TrafficLight exposing (TrafficLight, TrafficLights)
import Model.World exposing (World)
import Point2d
import QuadTree
import Quantity
import Vector2d



--
-- Tilemap
--


canBuildRoadAt : Cell -> World -> Bool
canBuildRoadAt cell world =
    let
        withinAllowedComplexity l =
            List.length l < 3

        hasLowComplexity diagonalDirection =
            Tilemap.cornerCells diagonalDirection cell
                |> List.filterMap (\c -> Tilemap.tileAt c world.tilemap)
                |> withinAllowedComplexity
    in
    List.all hasLowComplexity Tilemap.diagonalDirections


buildRoadAt : Cell -> World -> World
buildRoadAt cell world =
    updateTilemap cell (Tilemap.addTile cell) world


removeRoadAt : Cell -> World -> World
removeRoadAt cell world =
    updateTilemap cell (Tilemap.removeTile cell) world


updateTilemap : Cell -> (Tilemap -> Tilemap) -> World -> World
updateTilemap cell tilemapChangeFn world =
    let
        nextTilemap =
            tilemapChangeFn world.tilemap

        nextLots =
            Dict.filter
                (\_ lot ->
                    hasValidAnchorCell nextTilemap lot && not (Lot.inBounds cell lot)
                )
                world.lots
    in
    { world
        | tilemap = nextTilemap
        , lots = nextLots
    }
        |> updateRoadNetwork


hasValidAnchorCell : Tilemap -> Lot -> Bool
hasValidAnchorCell tilemap lot =
    case Tilemap.tileAt lot.anchor.anchorCell tilemap of
        Just tile ->
            tile == Tilemap.horizontalRoad || tile == Tilemap.verticalRoad

        Nothing ->
            False



--
-- Road network
--


innerLaneOffset : Length
innerLaneOffset =
    -- the distance from a road tile's edge to the inner lane (from left / bottom side)
    pixelsToMeters 26


outerLaneOffset : Length
outerLaneOffset =
    -- the distance from a road tile's edge to the outer lane (from the left / bottom side)
    pixelsToMeters 54


connectLotToRoadNetwork : World -> World
connectLotToRoadNetwork =
    -- Room for improvement: re-building the whole roadnetwork when a new lot is added is not optimal
    updateRoadNetwork


updateRoadNetwork : World -> World
updateRoadNetwork world =
    -- Room for improvement: the road network should be updated with minimal changes instead of being replaced
    let
        ( nextRoadNetwork, nextTrafficLights ) =
            buildRoadNetwork world

        nextRoadNetworkLookup =
            roadNetworkLookup nextRoadNetwork
    in
    { world
        | roadNetwork = nextRoadNetwork
        , trafficLights = nextTrafficLights
        , roadNetworkLookup = nextRoadNetworkLookup
    }


buildRoadNetwork : World -> ( RoadNetwork, TrafficLights )
buildRoadNetwork { tilemap, lots, trafficLights } =
    let
        tilePriority ( _, tile ) =
            if Tilemap.isDeadend tile then
                0

            else if Tilemap.isIntersection tile then
                1

            else
                2

        nodes =
            createConnections
                { tilemap = tilemap
                , lots = lots
                , nodes = Dict.empty
                , remainingTiles =
                    tilemap
                        |> Tilemap.toList Tuple.pair
                        |> List.sortBy tilePriority
                }
                |> Dict.values

        edges =
            createLanes nodes

        roadNetwork =
            Graph.fromNodesAndEdges nodes edges
    in
    roadNetwork |> setupTrafficControl trafficLights


findNodeReplacement : World -> RNNodeContext -> Maybe Int
findNodeReplacement { roadNetworkLookup } target =
    -- Tries to find a node in the lookup tree that could replace the reference node.
    -- Useful in maintaning route after the road network has been updated.
    let
        targetPosition =
            target.node.label.position

        positionMatches =
            roadNetworkLookup
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



-- Connections


type alias NodesMemo =
    -- Index nodes by their position (x, y) to avoid duplicates
    Dict ( Float, Float ) (Node Connection)


createConnections :
    { tilemap : Tilemap
    , lots : Dict Int Lot
    , nodes : NodesMemo
    , remainingTiles : List ( Cell, Tile )
    }
    -> NodesMemo
createConnections { nodes, tilemap, remainingTiles, lots } =
    case remainingTiles of
        [] ->
            nodes

        ( cell, tile ) :: otherTiles ->
            let
                connectionsInTile =
                    toConnections tilemap cell tile lots

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
                , lots = lots
                , nodes =
                    connectionsInTile
                        |> List.foldl maybeCreateNode nodes
                , remainingTiles = otherTiles
                }


toConnections : Tilemap -> Cell -> Tile -> Lots -> List Connection
toConnections tilemap cell tile lots =
    if tile == Tilemap.horizontalRoad then
        lotConnections cell tile Geometry.right lots

    else if tile == Tilemap.verticalRoad then
        lotConnections cell tile Geometry.up lots

    else if Tilemap.isDeadend tile then
        Tilemap.potentialConnections tile
            |> List.concatMap (Tilemap.orthogonalDirectionToLmDirection >> deadendConnections cell tile)

    else
        Tilemap.potentialConnections tile
            |> List.concatMap (connectionsByTileEntryDirection tilemap cell tile)


lotConnections : Cell -> Tile -> LMDirection2d -> Lots -> List Connection
lotConnections cell tile trafficDirection lots =
    case Dict.find (\_ lot -> lot.anchor.anchorCell == cell) lots of
        Just ( id, lot ) ->
            let
                ( posA, posB ) =
                    laneCenterPositionsByDirection cell trafficDirection

                anchorDirection =
                    lot.anchor.anchorDirection
                        |> Tilemap.orthogonalDirectionToLmDirection

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
            tileSize
                |> Quantity.half
                |> Quantity.minus innerLaneOffset

        tileCenterPosition =
            Tilemap.cellCenterPoint cell
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
            Tilemap.cellBottomLeftCorner cell

        isStopgapTile =
            Tilemap.nextOrthogonalCell direction cell
                |> Maybe.andThen (\nextCell -> Tilemap.tileAt nextCell tilemap)
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
              , position = shift outerLaneOffset tileSize
              , direction = Geometry.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift innerLaneOffset tileSize
              , direction = Geometry.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Right ->
            [ { kind = startConnectionKind
              , position = shift tileSize innerLaneOffset
              , direction = Geometry.right
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift tileSize outerLaneOffset
              , direction = Geometry.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Down ->
            [ { kind = startConnectionKind
              , position = shift innerLaneOffset Quantity.zero
              , direction = Geometry.down
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift outerLaneOffset Quantity.zero
              , direction = Geometry.up
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            ]

        Left ->
            [ { kind = startConnectionKind
              , position = shift Quantity.zero outerLaneOffset
              , direction = Geometry.left
              , cell = cell
              , tile = tile
              , trafficControl = None
              , lotId = Nothing
              }
            , { kind = endConnectionKind
              , position = shift Quantity.zero innerLaneOffset
              , direction = Geometry.right
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
    Tilemap.isCurve tile || Tilemap.isIntersection tile



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
                if current.id /= other.id && endsEdgeInsideCell other && connectsWithinCell current other then
                    Just (connect current other)

                else
                    Nothing
            )


endsEdgeInsideCell : Node Connection -> Bool
endsEdgeInsideCell node =
    node.label.kind == LaneStart || node.label.kind == Stopgap || node.label.kind == DeadendExit


connectsWithinCell : Node Connection -> Node Connection -> Bool
connectsWithinCell current other =
    let
        ( fromDir, toDir ) =
            ( connectionDirection current, connectionDirection other )

        range =
            Quantity.half tileSize

        target =
            connectionPosition other

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
            Direction2d.rotateCounterclockwise (connectionDirection node)
    in
    BoundingBox2d.translateIn leftDir range bb


connectionLookupAreaToRight : Node Connection -> Length -> LMBoundingBox2d
connectionLookupAreaToRight node range =
    let
        origin =
            connectionPosition node

        nodeDirection =
            connectionDirection node

        nodeDirectionRotatedRight =
            Direction2d.rotateClockwise nodeDirection

        otherCorner =
            origin
                |> Point2d.translateIn nodeDirection tileSize
                |> Point2d.translateIn nodeDirectionRotatedRight range
    in
    BoundingBox2d.from origin otherCorner


connectDeadendEntryWithExit : Node Connection -> Maybe (Edge Lane)
connectDeadendEntryWithExit entry =
    -- an assumption about node creation order (implied ID) is a cheap way to create the edge
    -- Room for improvement: really try to find a node that is at the expected Position
    Just { from = entry.id, to = entry.id + 1, label = () }



--
-- Traffic control
--


setupTrafficControl : TrafficLights -> RoadNetwork -> ( RoadNetwork, TrafficLights )
setupTrafficControl currentTrafficLights roadNetwork =
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


isOnPriorityRoad : RoadNetwork -> RNNodeContext -> Bool
isOnPriorityRoad roadNetwork nodeCtx =
    let
        otherNodeCtxs =
            RoadNetwork.getOutgoingConnections nodeCtx
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



--
-- Utility
--


connectionPosition : Node Connection -> LMPoint2d
connectionPosition node =
    node.label.position


connectionDirection : Node Connection -> LMDirection2d
connectionDirection node =
    node.label.direction
