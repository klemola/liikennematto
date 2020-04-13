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
    [ ( ( 5, 1 ), TwoLaneRoad (Deadend Up) )
    , ( ( 1, 2 ), TwoLaneRoad (Curve TopLeft) )
    , ( ( 2, 2 ), TwoLaneRoad Horizontal )
    , ( ( 3, 2 ), TwoLaneRoad Horizontal )
    , ( ( 4, 2 ), TwoLaneRoad Horizontal )
    , ( ( 6, 2 ), TwoLaneRoad Horizontal )
    , ( ( 7, 2 ), TwoLaneRoad Horizontal )
    , ( ( 8, 2 ), TwoLaneRoad (Curve TopRight) )
    , ( ( 1, 3 ), TwoLaneRoad Vertical )
    , ( ( 5, 3 ), TwoLaneRoad Vertical )
    , ( ( 8, 3 ), TwoLaneRoad Vertical )
    , ( ( 1, 4 ), TwoLaneRoad Vertical )
    , ( ( 5, 4 ), TwoLaneRoad Vertical )
    , ( ( 8, 4 ), TwoLaneRoad Vertical )
    , ( ( 2, 5 ), TwoLaneRoad Horizontal )
    , ( ( 3, 5 ), TwoLaneRoad Horizontal )
    , ( ( 4, 5 ), TwoLaneRoad Horizontal )
    , ( ( 6, 5 ), TwoLaneRoad Horizontal )
    , ( ( 7, 5 ), TwoLaneRoad Horizontal )
    , ( ( 8, 5 ), TwoLaneRoad (Curve BottomRight) )
    , ( ( 1, 6 ), TwoLaneRoad Vertical )
    , ( ( 5, 6 ), TwoLaneRoad Vertical )
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
    [ ( ( 5, 2 ), Intersection (Signal trafficLights) Crossroads )
    , ( ( 1, 5 ), Intersection Yield (T Right) )
    , ( ( 5, 5 ), Intersection Stop Crossroads )
    ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, Car ( 1, 7 ) Up Sedan1 Moving )
        , ( 2, Car ( 3, 5 ) Left Sedan2 Moving )
        , ( 3, Car ( 4, 5 ) Right Sedan3 Moving )
        , ( 4, Car ( 5, 1 ) Down Sedan4 Moving )
        ]
