module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..))
import Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
import TrafficLight


boardSize : Int
boardSize =
    9


tileSize : Float
tileSize =
    72


boardSizePx : Int
boardSizePx =
    boardSize * floor tileSize


initialRoads : List ( Coords, Tile )
initialRoads =
    [ ( ( 1, 2 ), TwoLaneRoad (Curve TopLeft) )
    , ( ( 2, 2 ), TwoLaneRoad Horizontal )
    , ( ( 3, 2 ), TwoLaneRoad Horizontal )
    , ( ( 4, 2 ), TwoLaneRoad Horizontal )
    , ( ( 6, 2 ), TwoLaneRoad Horizontal )
    , ( ( 7, 2 ), TwoLaneRoad (Deadend Right) )
    , ( ( 9, 1 ), TwoLaneRoad (Deadend Up) )
    , ( ( 9, 2 ), TwoLaneRoad Vertical )
    , ( ( 1, 3 ), TwoLaneRoad Vertical )
    , ( ( 5, 3 ), TwoLaneRoad Vertical )
    , ( ( 9, 3 ), TwoLaneRoad Vertical )
    , ( ( 1, 4 ), TwoLaneRoad Vertical )
    , ( ( 5, 4 ), TwoLaneRoad Vertical )
    , ( ( 9, 4 ), TwoLaneRoad Vertical )
    , ( ( 2, 5 ), TwoLaneRoad Horizontal )
    , ( ( 3, 5 ), TwoLaneRoad Horizontal )
    , ( ( 4, 5 ), TwoLaneRoad Horizontal )
    , ( ( 6, 5 ), TwoLaneRoad Horizontal )
    , ( ( 7, 5 ), TwoLaneRoad Horizontal )
    , ( ( 8, 5 ), TwoLaneRoad Horizontal )
    , ( ( 1, 6 ), TwoLaneRoad Vertical )
    , ( ( 5, 6 ), TwoLaneRoad Vertical )
    , ( ( 9, 6 ), TwoLaneRoad Vertical )
    , ( ( 1, 7 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 2, 7 ), TwoLaneRoad Horizontal )
    , ( ( 4, 7 ), TwoLaneRoad Horizontal )
    , ( ( 5, 7 ), TwoLaneRoad (Curve BottomRight) )
    , ( ( 9, 7 ), TwoLaneRoad Vertical )
    , ( ( 3, 8 ), TwoLaneRoad Vertical )
    , ( ( 9, 8 ), TwoLaneRoad Vertical )
    , ( ( 3, 9 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 4, 9 ), TwoLaneRoad Horizontal )
    , ( ( 6, 9 ), TwoLaneRoad Horizontal )
    , ( ( 5, 9 ), TwoLaneRoad Horizontal )
    , ( ( 7, 9 ), TwoLaneRoad Horizontal )
    , ( ( 8, 9 ), TwoLaneRoad Horizontal )
    , ( ( 9, 9 ), TwoLaneRoad (Curve BottomRight) )
    ]


initialIntersections : List ( Coords, Tile )
initialIntersections =
    let
        trafficLights =
            Direction.orientations
                |> List.concatMap TrafficLight.fromTrafficDirection
    in
    [ ( ( 5, 2 ), Intersection Stop (T Down) )
    , ( ( 1, 5 ), Intersection Stop (T Right) )
    , ( ( 9, 5 ), Intersection Yield (T Left) )
    , ( ( 5, 5 ), Intersection (Signal trafficLights) Crossroads )
    , ( ( 3, 7 ), Intersection Yield (T Down) )
    ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, Car ( 1, 8 ) Up Sedan1 Moving )
        , ( 2, Car ( 8, 8 ) Left Sedan2 Moving )
        , ( 3, Car ( 5, 6 ) Down Sedan3 Moving )
        , ( 4, Car ( 2, 8 ) Right Sedan4 Moving )
        , ( 5, Car ( 1, 3 ) Right Sedan5 Moving )
        ]
