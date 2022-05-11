module Model.Lookup exposing
    ( CarPositionLookup
    , RNLookupTreeEntry
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
import Model.Tilemap as Tilemap exposing (Tilemap)
import QuadTree


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


type alias CarPositionLookup =
    LMQuadTree Car


type alias RoadNetworkLookup =
    LMQuadTree RNLookupTreeEntry


type alias RNLookupTreeEntry =
    { id : Int, position : LMPoint2d, boundingBox : LMBoundingBox2d }


carPositionLookup : Tilemap -> Dict Int Car -> CarPositionLookup
carPositionLookup tilemap cars =
    Tilemap.toQuadTree tilemap quadTreeLeafElementsAmount
        |> QuadTree.insertList (Dict.values cars)


roadNetworkLookup : Tilemap -> RoadNetwork -> RoadNetworkLookup
roadNetworkLookup tilemap roadNetwork =
    Tilemap.toQuadTree tilemap quadTreeLeafElementsAmount
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
