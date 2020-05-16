module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..), Orientation(..))
import Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
import TrafficLight exposing (TrafficLight)


boardSize : Int
boardSize =
    9


tileSize : Float
tileSize =
    72


defaultTrafficLights : List TrafficLight
defaultTrafficLights =
    Direction.byOrientation
        |> List.concatMap TrafficLight.fromTrafficDirection


constructionTileGroups =
    { main =
        [ TwoLaneRoad (Regular Horizontal)
        , TwoLaneRoad (Regular Vertical)
        ]
    , intersectionCross =
        [ Intersection (Signal defaultTrafficLights) Crossroads
        ]
    , intersectionT =
        [ Intersection (Yield Vertical) (T Up)
        , Intersection (Yield Horizontal) (T Right)
        , Intersection (Yield Vertical) (T Down)
        , Intersection (Yield Horizontal) (T Left)
        ]
    , curve =
        [ TwoLaneRoad (Curve TopLeft)
        , TwoLaneRoad (Curve TopRight)
        , TwoLaneRoad (Curve BottomLeft)
        , TwoLaneRoad (Curve BottomRight)
        ]
    , deadend =
        [ TwoLaneRoad (Deadend Up)
        , TwoLaneRoad (Deadend Right)
        , TwoLaneRoad (Deadend Down)
        , TwoLaneRoad (Deadend Left)
        ]
    }


initialRoads : List ( Coords, Tile )
initialRoads =
    [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) )
    , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 1 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 9, 1 ), TwoLaneRoad (Deadend Right) )
    , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 2 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 2, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 3 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 9, 3 ), TwoLaneRoad (Curve TopRight) )
    , ( ( 1, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 4 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 5 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 5 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 5 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 6 ), TwoLaneRoad (Curve BottomLeft) )
    , ( ( 2, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 6 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 5, 7 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 7 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 5, 8 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 9, 8 ), TwoLaneRoad (Regular Vertical) )
    , ( ( 1, 9 ), TwoLaneRoad (Deadend Left) )
    , ( ( 2, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 3, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 4, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 6, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 7, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 8, 9 ), TwoLaneRoad (Regular Horizontal) )
    , ( ( 9, 9 ), TwoLaneRoad (Curve BottomRight) )
    ]


initialIntersections : List ( Coords, Tile )
initialIntersections =
    [ ( ( 5, 1 ), Intersection (Yield Vertical) (T Down) )
    , ( ( 1, 3 ), Intersection (Yield Horizontal) (T Right) )
    , ( ( 5, 3 ), Intersection (Stop Vertical) Crossroads )
    , ( ( 5, 6 ), Intersection (Signal defaultTrafficLights) Crossroads )
    , ( ( 9, 6 ), Intersection (Stop Horizontal) (T Left) )
    , ( ( 5, 9 ), Intersection (Yield Vertical) (T Up) )
    ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, Car.new Sedan1 )
        , ( 2, Car.new Sedan2 )
        , ( 3, Car.new Sedan3 )
        , ( 4, Car.new Sedan4 )
        , ( 5, Car.new Sedan5 )
        ]
