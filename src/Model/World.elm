module Model.World exposing
    ( World
    , empty
    , findNodeByPosition
    , hasLot
    , isEmptyArea
    , removeCar
    , removeLot
    , setCar
    , setLot
    )

import BoundingBox2d
import Common
import Dict exposing (Dict)
import Graph
import Length
import Model.Car as Car exposing (Car)
import Model.Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d)
import Model.Lookup
    exposing
        ( CarPositionLookup
        , RoadNetworkLookup
        , carPositionLookup
        , roadNetworkLookup
        )
import Model.Lot as Lot exposing (Lot)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext, RoadNetwork)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.TrafficLight exposing (TrafficLights)
import QuadTree
import Set


type alias World =
    { tilemap : Tilemap
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Dict Id Car
    , lots : Dict Id Lot
    , carPositionLookup : CarPositionLookup
    , roadNetworkLookup : RoadNetworkLookup
    }


empty : Tilemap.TilemapConfig -> World
empty tilemapConfig =
    let
        tilemap =
            Tilemap.empty tilemapConfig
    in
    { tilemap = tilemap
    , roadNetwork = RoadNetwork.empty
    , trafficLights = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    , carPositionLookup = carPositionLookup tilemap Dict.empty
    , roadNetworkLookup = roadNetworkLookup tilemap Graph.empty
    }



--
-- Queries
--


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        tilemapOverlap =
            Tilemap.intersects testAreaBB world.tilemap

        lotOverlap =
            Dict.foldl (\_ lot acc -> lot.boundingBox :: acc) [] world.lots
                |> List.any (Common.boundingBoxOverlaps testAreaBB)
    in
    Tilemap.inBounds world.tilemap testAreaBB && not lotOverlap && not tilemapOverlap


findNodeByPosition : World -> LMPoint2d -> Maybe RNNodeContext
findNodeByPosition { roadNetworkLookup, roadNetwork } nodePosition =
    roadNetworkLookup
        |> QuadTree.neighborsWithin
            (Length.meters 1)
            (BoundingBox2d.singleton nodePosition)
        |> List.head
        |> Maybe.andThen (.id >> RoadNetwork.findNodeByNodeId roadNetwork)



--
-- Utility
--


setCar : Car -> World -> World
setCar car world =
    { world | cars = Dict.insert car.id car world.cars }


removeCar : Id -> World -> World
removeCar carId world =
    { world | cars = Dict.remove carId world.cars }


setLot : Lot -> World -> World
setLot lot world =
    { world | lots = Dict.insert lot.id lot world.lots }


removeLot : Id -> World -> World
removeLot lotId world =
    case Dict.get lotId world.lots of
        Just lot ->
            let
                parkedCarsIds =
                    lot.parkingSpots
                        |> List.filterMap (\parkingSpot -> parkingSpot.reservedBy)
                        |> Set.fromList

                nextCars =
                    Dict.map
                        (\_ car ->
                            let
                                isParkedOnLot =
                                    Set.member car.id parkedCarsIds

                                homeRemoved =
                                    case car.homeLotId of
                                        Just homeLotId ->
                                            homeLotId == lotId

                                        Nothing ->
                                            False
                            in
                            if isParkedOnLot || homeRemoved then
                                Car.triggerDespawn car

                            else
                                car
                        )
                        world.cars
            in
            { world
                | lots = Dict.remove lotId world.lots
                , tilemap = Tilemap.removeAnchor lotId world.tilemap
                , cars = nextCars
            }

        Nothing ->
            world
