module Model.RoadNetwork exposing
    ( Connection
    , ConnectionKind(..)
    , Lane
    , RNNodeContext
    , RoadNetwork
    , TrafficControl(..)
    , new
    , toDotString
    )

import Graph exposing (Graph, NodeContext)
import Graph.DOT
import Model.Board exposing (Tile)
import Model.Cell exposing (Cell, OrthogonalDirection(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMDirection2d, LMPoint2d)


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


new : RoadNetwork
new =
    Graph.empty


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
