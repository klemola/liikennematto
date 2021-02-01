module Tile exposing
    ( IntersectionControl(..)
    , IntersectionShape(..)
    , Orientation(..)
    , RoadKind(..)
    , Tile(..)
    , TrafficDirection(..)
    , defaultIntersectionControl
    , fromId
    , oppositeOrientation
    , potentialConnections
    , setIntersectionControl
    , toOrientation
    , toggleIntersectionControl
    , toggleTrafficDirection
    )

import Cell exposing (Corner(..), OrthogonalDirection(..))
import Dict exposing (Dict)
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad RoadKind TrafficDirection
    | Intersection IntersectionControl IntersectionShape


type Orientation
    = Vertical
    | Horizontal


type RoadKind
    = Regular Orientation
    | Curve Corner
    | Deadend OrthogonalDirection


type TrafficDirection
    = Both
    | OneWay


type IntersectionControl
    = Signal TrafficLights
    | Yield Orientation
    | Stop Orientation


type IntersectionShape
    = T OrthogonalDirection
    | Crossroads


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections tile =
    case tile of
        TwoLaneRoad (Regular orientation) _ ->
            if orientation == Vertical then
                [ Up, Down ]

            else
                [ Left, Right ]

        TwoLaneRoad (Curve TopRight) _ ->
            [ Left, Down ]

        TwoLaneRoad (Curve TopLeft) _ ->
            [ Right, Down ]

        TwoLaneRoad (Curve BottomRight) _ ->
            [ Left, Up ]

        TwoLaneRoad (Curve BottomLeft) _ ->
            [ Right, Up ]

        TwoLaneRoad (Deadend dir) _ ->
            [ Cell.oppositeOrthogonalDirection dir ]

        Intersection _ Crossroads ->
            Cell.allODs

        Intersection _ (T dir) ->
            dir :: Cell.crossOrthogonalDirection dir


toggleIntersectionControl : Tile -> Tile
toggleIntersectionControl tile =
    case tile of
        -- Crossroads intersections cycle from signal to stop to yield (and back to signal)
        Intersection (Signal _) Crossroads ->
            Intersection (Stop Vertical) Crossroads

        Intersection (Stop Vertical) Crossroads ->
            -- swap orientation
            Intersection (Stop Horizontal) Crossroads

        Intersection (Stop Horizontal) Crossroads ->
            Intersection (Yield Vertical) Crossroads

        Intersection (Yield Vertical) Crossroads ->
            -- swap orientation
            Intersection (Yield Horizontal) Crossroads

        Intersection (Yield Horizontal) Crossroads ->
            Intersection (Signal TrafficLight.default) Crossroads

        -- T shape intersections can't have traffic lights and their orientation is based on the main road
        -- toggling between them is simple - from stop to yield and back
        Intersection (Stop orientation) (T dir) ->
            Intersection (Yield orientation) (T dir)

        Intersection (Yield orientation) (T dir) ->
            Intersection (Stop orientation) (T dir)

        _ ->
            tile


toOrientation : OrthogonalDirection -> Orientation
toOrientation dir =
    case dir of
        Up ->
            Vertical

        Right ->
            Horizontal

        Down ->
            Vertical

        Left ->
            Horizontal


oppositeOrientation : Orientation -> Orientation
oppositeOrientation orientation =
    case orientation of
        Vertical ->
            Horizontal

        Horizontal ->
            Vertical


defaultIntersectionControl : IntersectionShape -> IntersectionControl
defaultIntersectionControl shape =
    case shape of
        T dir ->
            toOrientation dir
                |> oppositeOrientation
                |> Yield

        Crossroads ->
            Signal TrafficLight.default


setIntersectionControl : Tile -> IntersectionControl -> Tile
setIntersectionControl tile control =
    case tile of
        Intersection _ shape ->
            Intersection control shape

        _ ->
            tile


toggleTrafficDirection : Tile -> Tile
toggleTrafficDirection tile =
    case tile of
        TwoLaneRoad kind OneWay ->
            TwoLaneRoad kind Both

        TwoLaneRoad kind Both ->
            TwoLaneRoad kind OneWay

        _ ->
            tile


ids : Dict Int Tile
ids =
    Dict.fromList
        [ -- value for "0" is omitted - does not represent any road piece
          ( 1, TwoLaneRoad (Deadend Down) Both )
        , ( 2, TwoLaneRoad (Deadend Right) Both )
        , ( 3, TwoLaneRoad (Curve BottomRight) Both )
        , ( 4, TwoLaneRoad (Deadend Left) Both )
        , ( 5, TwoLaneRoad (Curve BottomLeft) Both )
        , ( 6, TwoLaneRoad (Regular Horizontal) Both )
        , ( 7, Intersection (Yield Vertical) (T Up) )
        , ( 8, TwoLaneRoad (Deadend Up) Both )
        , ( 9, TwoLaneRoad (Regular Vertical) Both )
        , ( 10, TwoLaneRoad (Curve TopRight) Both )
        , ( 11, Intersection (Yield Horizontal) (T Left) )
        , ( 12, TwoLaneRoad (Curve TopLeft) Both )
        , ( 13, Intersection (Yield Horizontal) (T Right) )
        , ( 14, Intersection (Yield Vertical) (T Down) )
        , ( 15, Intersection (Signal TrafficLight.default) Crossroads )
        ]


fromId : Int -> Maybe Tile
fromId fourBitId =
    Dict.get fourBitId ids
