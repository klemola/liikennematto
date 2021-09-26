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
import Model.Board as Board exposing (Board, Cell)
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
import Model.TrafficLight exposing (TrafficLights)


type alias World =
    { board : Board
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Cars
    , lots : Lots
    , carPositionLookup : CarPositionLookup
    , roadNetworkLookup : RoadNetworkLookup
    }


empty : World
empty =
    { board = Board.empty
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
    List.any (\lot -> Lot.anchorCell lot == cell) (Dict.values lots)


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        roadBoundingBoxes =
            world.board
                |> Dict.keys
                |> List.map Board.cellBoundingBox

        lotBoundingBoxes =
            world.lots
                |> Dict.values
                |> List.map Lot.boundingBox

        inBoardBounds =
            Board.inBounds testAreaBB

        noCollision _ =
            (roadBoundingBoxes ++ lotBoundingBoxes)
                |> List.all (Common.noBoundingBoxOverlap testAreaBB)
    in
    inBoardBounds && noCollision ()



--
-- Utility
--


setCar : Id -> Car -> World -> World
setCar id car world =
    { world | cars = Dict.insert id car world.cars }
