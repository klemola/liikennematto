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
    , getOutgoingConnections
    , getRandomNode
    , size
    , toDotString
    , trafficControl
    )

import Graph exposing (Graph, NodeContext, NodeId)
import Graph.DOT
import Model.Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)
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
    , environment : ConnectionEnvironment
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
    | Yield
    | NoTrafficControl


type alias Lane =
    ()


empty : RoadNetwork
empty =
    Graph.empty



--
-- Queries
--


size : RoadNetwork -> Int
size =
    Graph.size


findLotExitNodeByLotId : RoadNetwork -> Int -> Maybe RNNodeContext
findLotExitNodeByLotId roadNetwork lotId =
    roadNetwork
        |> Graph.fold
            (\ctx acc ->
                case ctx.node.label.kind of
                    LotExit id ->
                        if id == lotId then
                            Just ctx

                        else
                            acc

                    _ ->
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
                |> Random.map (Maybe.andThen (findNodeByNodeId roadNetwork))
    in
    Random.step randomNodeGenerator seed


getOutgoingConnections : RNNodeContext -> List NodeId
getOutgoingConnections nodeCtx =
    Graph.alongOutgoingEdges nodeCtx


trafficControl : RNNodeContext -> TrafficControl
trafficControl nodeCtx =
    nodeCtx.node.label.trafficControl



--
-- Debug
--


connectionKindToString : ConnectionKind -> String
connectionKindToString kind =
    case kind of
        LaneConnector ->
            "Lane connector"

        DeadendEntry ->
            "Deadend entry"

        DeadendExit ->
            "Deadend exit"

        LotEntry id ->
            "Lot entry #" ++ String.fromInt id

        LotExit id ->
            "Lot exit #" ++ String.fromInt id


toDotString : RoadNetwork -> String
toDotString =
    let
        nodeFormatter =
            \connection -> Just (connectionKindToString connection.kind)

        edgeFormatter =
            always Nothing
    in
    Graph.DOT.output nodeFormatter edgeFormatter
