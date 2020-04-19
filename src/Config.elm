module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
import TrafficLight


boardSize : Int
boardSize =
    8


tileSize : Float
tileSize =
    80


boardSizePx : Int
boardSizePx =
    boardSize * floor tileSize


initialRoads : List ( Coords, Tile )
initialRoads =
    [ ( ( 2, 1 ), TwoLaneRoad (Deadend Left) )
    , ( ( 3, 1 ), TwoLaneRoad Horizontal )
    , ( ( 4, 1 ), TwoLaneRoad Horizontal )
    , ( ( 5, 1 ), TwoLaneRoad (Curve TopRight) )
    , ( ( 5, 2 ), TwoLaneRoad Vertical )
    , ( ( 1, 3 ), TwoLaneRoad (Curve TopLeft) )
    , ( ( 2, 3 ), TwoLaneRoad Horizontal )
    , ( ( 3, 3 ), TwoLaneRoad Horizontal )
    , ( ( 4, 3 ), TwoLaneRoad Horizontal )
    , ( ( 6, 3 ), TwoLaneRoad Horizontal )
    , ( ( 7, 3 ), TwoLaneRoad Horizontal )
    , ( ( 8, 3 ), TwoLaneRoad (Curve TopRight) )
    , ( ( 1, 4 ), TwoLaneRoad Vertical )
    , ( ( 5, 4 ), TwoLaneRoad Vertical )
    , ( ( 8, 4 ), TwoLaneRoad Vertical )
    , ( ( 1, 5 ), TwoLaneRoad Vertical )
    , ( ( 5, 5 ), TwoLaneRoad Vertical )
    , ( ( 8, 5 ), TwoLaneRoad Vertical )
    , ( ( 2, 6 ), TwoLaneRoad Horizontal )
    , ( ( 3, 6 ), TwoLaneRoad Horizontal )
    , ( ( 4, 6 ), TwoLaneRoad Horizontal )
    , ( ( 6, 6 ), TwoLaneRoad Horizontal )
    , ( ( 7, 6 ), TwoLaneRoad Horizontal )
    , ( ( 8, 6 ), TwoLaneRoad (Curve BottomRight) )
    , ( ( 1, 7 ), TwoLaneRoad Vertical )
    , ( ( 5, 7 ), TwoLaneRoad Vertical )
    , ( ( 1, 8 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 2, 8 ), TwoLaneRoad Horizontal )
    , ( ( 3, 8 ), TwoLaneRoad Horizontal )
    , ( ( 4, 8 ), TwoLaneRoad Horizontal )
    , ( ( 5, 8 ), TwoLaneRoad (Curve BottomRight) )
    ]


initialIntersections : List ( Coords, Tile )
initialIntersections =
    let
        trafficLights =
            Direction.orientations
                |> List.concatMap TrafficLight.fromTrafficDirection
    in
    [ ( ( 5, 3 ), Intersection Stop Crossroads )
    , ( ( 1, 6 ), Intersection Yield (T Right) )
    , ( ( 5, 6 ), Intersection (Signal trafficLights) Crossroads )
    ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, Car ( 1, 8 ) Up Sedan1 Moving )
        , ( 2, Car ( 3, 6 ) Left Sedan2 Moving )
        , ( 3, Car ( 4, 6 ) Right Sedan3 Moving )
        , ( 4, Car ( 2, 1 ) Right Sedan4 Moving )
        ]
