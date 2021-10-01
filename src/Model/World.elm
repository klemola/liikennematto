module Model.World exposing
    ( World
    , empty
    , hasLot
    , hasLotAnchor
    , isEmptyArea
    , setCar
    )

import Common
import Dict
import Graph
import Model.Car exposing (Car, Cars)
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMBoundingBox2d)
import Model.Lookup
    exposing
        ( CarPositionLookup
        , RoadNetworkLookup
        , carPositionLookup
        , roadNetworkLookup
        )
import Model.Lot as Lot exposing (Lots)
import Model.RoadNetwork as RoadNetwork exposing (RoadNetwork)
import Model.Tilemap as Tilemap exposing (Cell, Tilemap)
import Model.TrafficLight exposing (TrafficLights)


type alias World =
    { tilemap : Tilemap
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Cars
    , lots : Lots
    , carPositionLookup : CarPositionLookup
    , roadNetworkLookup : RoadNetworkLookup
    }


empty : World
empty =
    { tilemap = Tilemap.empty
    , roadNetwork = RoadNetwork.empty
    , trafficLights = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    , carPositionLookup = carPositionLookup Dict.empty
    , roadNetworkLookup = roadNetworkLookup Graph.empty
    }



--
-- Queries
--


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


hasLotAnchor : Cell -> World -> Bool
hasLotAnchor cell { lots } =
    List.any (\lot -> lot.anchor.anchorCell == cell) (Dict.values lots)


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        roadBoundingBoxes =
            -- Room for improvement: provide tilemap iteration from the Tilemap module with better guarantees
            world.tilemap
                |> Dict.keys
                |> List.filterMap
                    (\cellCoordinates ->
                        Tilemap.cellFromCoordinates cellCoordinates
                            |> Maybe.map Tilemap.cellBoundingBox
                    )

        lotBoundingBoxes =
            Dict.foldl (\_ lot acc -> lot.boundingBox :: acc) [] world.lots

        inTilemapBounds =
            Tilemap.inBounds testAreaBB

        noCollision =
            (roadBoundingBoxes ++ lotBoundingBoxes)
                |> List.all (Common.noBoundingBoxOverlap testAreaBB)
    in
    inTilemapBounds && noCollision



--
-- Utility
--


setCar : Id -> Car -> World -> World
setCar id car world =
    { world | cars = Dict.insert id car world.cars }
