module Model.World exposing
    ( World
    , canBuildRoadAt
    , empty
    , hasLot
    , hasLotAnchor
    , isEmptyArea
    , tileAt
    )

import Common
import Dict
import Dict.Extra
import Model.Board as Board exposing (Board, Tile)
import Model.Car exposing (Cars)
import Model.Cell as Cell exposing (Cell)
import Model.Geometry exposing (LMBoundingBox2d)
import Model.Lot as Lot exposing (BuildingKind(..), Lots)
import Model.RoadNetwork as RoadNetwork exposing (RoadNetwork)
import Model.TrafficLight exposing (TrafficLights)


type alias World =
    { board : Board
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Cars
    , lots : Lots
    }


empty : World
empty =
    { board = Dict.empty
    , roadNetwork = RoadNetwork.new
    , trafficLights = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    }


tileAt : Cell -> World -> Maybe Tile
tileAt cell { board } =
    board
        |> Dict.Extra.find (\key _ -> key == cell)
        |> Maybe.map Tuple.second


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


hasLotAnchor : Cell -> World -> Bool
hasLotAnchor cell { lots } =
    List.any (\lot -> Lot.anchorCell lot == cell) (Dict.values lots)


canBuildRoadAt : Cell -> World -> Bool
canBuildRoadAt cell world =
    let
        withinAllowedComplexity l =
            List.length l < 3

        hasLowComplexity corner =
            Cell.cornerAndNeighbors corner cell
                |> List.filterMap (\c -> tileAt c world)
                |> withinAllowedComplexity
    in
    List.all hasLowComplexity Cell.corners


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        roadBoundingBoxes =
            world.board
                |> Dict.keys
                |> List.map Cell.boundingBox

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
