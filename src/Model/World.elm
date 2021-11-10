module Model.World exposing
    ( World
    , addLot
    , empty
    , hasLot
    , hasLotAnchor
    , isEmptyArea
    , removeCar
    , removeLot
    , setCar
    )

import Common
import Dict exposing (Dict)
import Graph
import Model.Car as Car exposing (Car, CarKind(..))
import Model.Entity as Entity exposing (Id)
import Model.Geometry exposing (LMBoundingBox2d)
import Model.Lookup
    exposing
        ( CarPositionLookup
        , RoadNetworkLookup
        , carPositionLookup
        , roadNetworkLookup
        )
import Model.Lot as Lot exposing (Lot, Lots)
import Model.RoadNetwork as RoadNetwork exposing (RoadNetwork)
import Model.Tilemap as Tilemap exposing (Cell, Tilemap)
import Model.TrafficLight exposing (TrafficLights)


type alias World =
    { tilemap : Tilemap
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Dict Id Car
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
        tilemapOverlap =
            Tilemap.intersects testAreaBB world.tilemap

        lotOverlap =
            Dict.foldl (\_ lot acc -> lot.boundingBox :: acc) [] world.lots
                |> List.any (Common.boundingBoxOverlaps testAreaBB)
    in
    Tilemap.inBounds testAreaBB && not lotOverlap && not tilemapOverlap



--
-- Utility
--


setCar : Id -> Car -> World -> World
setCar carId car world =
    { world | cars = Dict.insert carId car world.cars }


removeCar : Id -> World -> World
removeCar carId world =
    { world | cars = Dict.remove carId world.cars }


addLot : Lot -> World -> ( Id, World )
addLot lot world =
    let
        nextLotId =
            Entity.nextId world.lots

        nextLots =
            Dict.insert nextLotId lot world.lots
    in
    ( nextLotId, { world | lots = nextLots } )


removeLot : Id -> World -> World
removeLot lotId world =
    let
        nextCars =
            Dict.map
                (\_ car ->
                    let
                        shouldReroute =
                            case car.homeLotId of
                                Just homeLotId ->
                                    homeLotId == lotId

                                Nothing ->
                                    False
                    in
                    if shouldReroute then
                        Car.triggerReroute car

                    else
                        car
                )
                world.cars
    in
    { world
        | lots = Dict.remove lotId world.lots
        , cars = nextCars
    }
