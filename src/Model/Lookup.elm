module Model.Lookup exposing
    ( CarPositionLookup
    , RoadNetworkLookup
    , carPositionLookup
    , roadNetworkLookup
    )

import BoundingBox2d
import Dict exposing (Dict)
import Graph
import Model.Car exposing (Car)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , LMQuadTree
        )
import Model.RoadNetwork exposing (RoadNetwork)
import Model.Tilemap as Tilemap
import QuadTree


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


type alias CarPositionLookup =
    LMQuadTree Car


type alias RoadNetworkLookup =
    LMQuadTree LookupTreeEntry


type alias LookupTreeEntry =
    { id : Int, position : LMPoint2d, boundingBox : LMBoundingBox2d }


carPositionLookup : Dict Int Car -> CarPositionLookup
carPositionLookup cars =
    QuadTree.init Tilemap.boundingBox quadTreeLeafElementsAmount
        |> QuadTree.insertList (Dict.values cars)


roadNetworkLookup : RoadNetwork -> RoadNetworkLookup
roadNetworkLookup roadNetwork =
    QuadTree.init Tilemap.boundingBox 4
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
