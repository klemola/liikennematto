module Model.Lookup exposing (CarPositionLookup, RoadNetworkLookup, carPositionLookup, roadNetworkLookup)

import BoundingBox2d
import Dict
import Graph
import Length
import Model.Car exposing (Car, Cars)
import Model.Geometry exposing (LMBoundingBox2d, LMEntityCoordinates, LMPoint2d)
import Model.RoadNetwork exposing (RoadNetwork)
import Model.Tilemap as Tilemap
import QuadTree exposing (QuadTree)


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


type alias CarPositionLookup =
    QuadTree Length.Meters LMEntityCoordinates Car


type alias RoadNetworkLookup =
    QuadTree Length.Meters LMEntityCoordinates LookupTreeEntry


type alias LookupTreeEntry =
    { id : Int, position : LMPoint2d, boundingBox : LMBoundingBox2d }


carPositionLookup : Cars -> CarPositionLookup
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
