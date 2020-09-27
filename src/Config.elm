module Config exposing (..)

import Car exposing (Car, CarKind(..), Status(..), TurnKind(..))
import Coords exposing (Coords)
import Dict exposing (Dict)
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Element exposing (rgb255, rgba255)
import Html.Attributes exposing (dir)
import Lot exposing (BuildingKind(..), Lot(..))
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight


boardSize : Int
boardSize =
    10


defaultTile =
    TwoLaneRoad (Regular Horizontal) Both


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
        [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 4 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 1, 7 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 10 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 7 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 4 ), Intersection (Yield Vertical) (T Down) )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 7 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 3, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 4, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 5, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 6, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 7, 1 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 7, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 4 ), Intersection (Signal TrafficLight.default) Crossroads )
        , ( ( 7, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 7 ), Intersection (Stop Horizontal) (T Left) )
        , ( ( 7, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 10 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 8, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 8, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 10, 4 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 10, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 7 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 10 ), TwoLaneRoad (Curve BottomRight) Both )
        ]


firstLot =
    Building ResidentialA ( ( 3, 10 ), Up )


firstCar =
    { kind = resident firstLot
    , coords = ( 3, 10 )
    , direction = Left
    , homeLotId = Just 1
    , status = Moving
    }


initialLots : Dict Int Lot
initialLots =
    Dict.fromList
        [ ( 1, firstLot )
        ]


initialCars : Dict Int Car
initialCars =
    Dict.fromList
        [ ( 1, firstCar )
        ]


resident : Lot -> CarKind
resident (Building buildingKind _) =
    case buildingKind of
        ResidentialA ->
            SedanA

        ResidentialB ->
            SedanB

        ResidentialC ->
            SedanC

        ResidentialD ->
            SedanD

        ResidentialE ->
            SedanE
