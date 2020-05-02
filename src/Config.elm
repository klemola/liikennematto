module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..), Orientation(..))
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
    , ( ( 2, 2 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 2 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 2 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 2 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 2 ), TwoLaneRoad (Deadend Right) )
    , ( ( 9, 1 ), TwoLaneRoad (Deadend Up) )
    , ( ( 9, 2 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 3 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 3 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 3 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 2, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 5 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 1, 6 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 6 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 6 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 7 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 2, 7 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 7 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 5, 7 ), TwoLaneRoad (Curve BottomRight) )
    , ( ( 9, 7 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 3, 8 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 8 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 3, 9 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 4, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 5, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 9, 9 ), TwoLaneRoad (Curve BottomRight) )
    ]


initialIntersections : List ( Coords, Tile )
initialIntersections =
    let
        trafficLights =
            Direction.byOrientation
                |> List.concatMap TrafficLight.fromTrafficDirection
    in
    [ ( ( 5, 2 ), Intersection (Stop Vertical) (T Down) )
    , ( ( 1, 5 ), Intersection (Stop Horizontal) (T Right) )
    , ( ( 9, 5 ), Intersection (Yield Horizontal) (T Left) )
    , ( ( 5, 5 ), Intersection (Signal trafficLights) Crossroads )
    , ( ( 3, 7 ), Intersection (Yield Vertical) (T Down) )
    ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, Car ( 1, 2 ) Down Sedan1 Moving )
        , ( 2, Car ( 9, 1 ) Down Sedan2 Moving )
        , ( 3, Car ( 6, 5 ) Left Sedan3 Moving )
        , ( 4, Car ( 3, 5 ) Left Sedan4 Moving )
        , ( 5, Car ( 9, 9 ) Up Sedan5 Moving )
        ]
