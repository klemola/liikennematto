module Model.RoadNetwork exposing
    ( Connection
    , ConnectionKind(..)
    , Lane
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , empty
    , findLotExitByNodeId
    , findNodeByNodeId
    , findNodeByPosition
    , getOutgoingConnections
    , getRandomNode
    , size
    , toDotString
    )

import Graph exposing (Graph, NodeContext, NodeId)
import Graph.DOT
import Model.Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)
import Model.Tile exposing (Tile)
import Random
import Random.Extra


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
    , trafficControl : TrafficControl
    }


type ConnectionKind
    = LaneConnector
    | DeadendEntry
    | DeadendExit
    | LotEntry Id
    | LotExit Id


type TrafficControl
    = Signal Id
    | Yield
    | None


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


findLotExitByNodeId : RoadNetwork -> Int -> Maybe RNNodeContext
findLotExitByNodeId roadNetwork lotId =
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
