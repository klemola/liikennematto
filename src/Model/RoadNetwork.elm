module Model.RoadNetwork exposing
    ( Connection
    , ConnectionKind(..)
    , Lane
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , empty
    , findNodeByLotId
    , findNodeByNodeId
    , findNodeByPosition
    , getOutgoingConnections
    , getRandomNode
    , toDotString
    )

import Graph exposing (Graph, NodeContext, NodeId)
import Graph.DOT
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)
import Model.Tilemap exposing (Cell, Tile)
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



--
-- Debug
--


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
