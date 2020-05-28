module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Direction(..), Orientation(..))
import Element exposing (rgb255, rgba255)
import Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..))
import TrafficLight


boardSize : Int
boardSize =
    10


constructionTileGroups =
    { main =
        [ TwoLaneRoad (Regular Horizontal)
        , TwoLaneRoad (Regular Vertical)
        ]
    , intersectionCross =
        [ Intersection (Signal TrafficLight.default) Crossroads
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


colors =
    { mainBackground = rgb255 68 115 120
    , toolbarBackground = rgb255 159 192 198
    , buttonBackground = rgb255 228 228 235
    , listItemBackground = rgb255 109 151 156
    , text = rgb255 52 65 67
    , textInverse = rgb255 222 222 222
    , link = rgb255 10 132 199
    , selected = rgb255 242 212 13
    , danger = rgb255 235 119 52
    , notAllowed = rgb255 245 66 84
    , target = rgb255 222 222 222
    , terrain = rgb255 33 191 154
    , transparent = rgba255 0 0 0 0
    , lightBorder = rgb255 220 220 226
    , heavyBorder = rgb255 53 93 97
    }


whitespace =
    { regular = 10
    , spacious = 20
    , tight = 5
    }


borderSize =
    { heavy = 10
    , light = 3
    }


borderRadius =
    { heavy = 5
    , light = 3
    }


initialBoard : Dict Coords Tile
initialBoard =
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 3 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 4 ), Intersection (Stop Horizontal) (T Right) )
        , ( ( 1, 5 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 6 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 7 ), Intersection (Yield Horizontal) (T Right) )
        , ( ( 1, 8 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 9 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 1, 10 ), TwoLaneRoad (Curve BottomLeft) )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 2, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 2, 7 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 2, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 3, 1 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 3, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 3, 7 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 3, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 4, 1 ), Intersection (Stop Vertical) (T Down) )
        , ( ( 4, 2 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 4, 3 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 4, 4 ), Intersection (Signal TrafficLight.default) Crossroads )
        , ( ( 4, 5 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 4, 6 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 4, 7 ), TwoLaneRoad (Curve BottomRight) )
        , ( ( 4, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 5, 1 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 5, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 6, 1 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 6, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 6, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 7, 1 ), TwoLaneRoad (Curve TopRight) )
        , ( ( 7, 2 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 3 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 4 ), Intersection (Yield Vertical) Crossroads )
        , ( ( 7, 5 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 6 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 7 ), Intersection (Yield Horizontal) (T Right) )
        , ( ( 7, 8 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 9 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 7, 10 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 8, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 8, 7 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 8, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 9, 4 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 9, 7 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 9, 10 ), TwoLaneRoad (Regular Horizontal) )
        , ( ( 10, 4 ), TwoLaneRoad (Curve TopRight) )
        , ( ( 10, 5 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 10, 6 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 10, 7 ), Intersection (Yield Horizontal) (T Left) )
        , ( ( 10, 8 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 10, 9 ), TwoLaneRoad (Regular Vertical) )
        , ( ( 10, 10 ), TwoLaneRoad (Curve BottomRight) )
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
