module Model.RoadNetwork exposing
    ( Connection
    , ConnectionEnvironment(..)
    , ConnectionKind(..)
    , Lane
    , RNNode
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , empty
    , findLotExitNodeByLotId
    , findNodeByNodeId
    , findNodeByPosition
    , getOutgoingConnectionIds
    , getOutgoingConnectionsAndCosts
    , getRandomNode
    , nodeDirection
    , nodeLotId
    , nodePosition
    , nodeTrafficControl
    , outgoingConnectionsAmount
    , size
    )

import Collection exposing (Id, idMatches)
import Graph exposing (Graph, NodeContext, NodeId)
import IntDict
import Length exposing (Length)
import Model.Cell exposing (Cell)
import Model.Geometry exposing (LMBoundingBox2d, LMDirection2d, LMPoint2d)
import Random
import Random.Extra


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
                |> Random.map (Maybe.andThen (findNodeByNodeId roadNetwork))
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
            case findNodeByNodeId roadNetwork k of
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
