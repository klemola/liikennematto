module Tile exposing
    ( IntersectionControl(..)
    , IntersectionShape(..)
    , RoadKind(..)
    , Tile(..)
    , TrafficDirection(..)
    , connected
    , defaultIntersectionControl
    , fromId
    , isRoad
    , priorityDirections
    , setIntersectionControl
    , toString
    , toggleIntersectionControl
    , toggleTrafficDirection
    )

import Dict exposing (Dict)
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad RoadKind TrafficDirection
    | Intersection IntersectionControl IntersectionShape


type RoadKind
    = Regular Orientation
    | Curve Corner
    | Deadend Direction


type TrafficDirection
    = Both
    | OneWay


type IntersectionControl
    = Signal TrafficLights
    | Yield Orientation
    | Stop Orientation
    | Uncontrolled


type IntersectionShape
    = T Direction
    | Crossroads


isRoad : Tile -> Bool
isRoad tile =
    case tile of
        TwoLaneRoad _ _ ->
            True

        _ ->
            False


priorityDirections : Tile -> List Direction
priorityDirections tile =
    let
        priority trafficControlOrientation =
            trafficControlOrientation
                |> Direction.oppositeOrientation
                |> Direction.fromOrientation
    in
    case tile of
        Intersection (Yield orientation) _ ->
            priority orientation

        Intersection (Stop orientation) _ ->
            priority orientation

        -- Other tile kinds don't have priority (e.g. signal controlled intersections)
        _ ->
            []


potentialConnections : Tile -> List Direction
potentialConnections tile =
    case tile of
        TwoLaneRoad (Regular orientation) _ ->
            Direction.fromOrientation orientation

        TwoLaneRoad (Curve TopRight) _ ->
            [ Left, Down ]

        TwoLaneRoad (Curve TopLeft) _ ->
            [ Right, Down ]

        TwoLaneRoad (Curve BottomRight) _ ->
            [ Left, Up ]

        TwoLaneRoad (Curve BottomLeft) _ ->
            [ Right, Up ]

        TwoLaneRoad (Deadend dir) _ ->
            [ Direction.opposite dir ]

        Intersection _ Crossroads ->
            Direction.all

        Intersection _ (T dir) ->
            dir :: Direction.cross dir


matchesTrafficDirection : Direction -> Tile -> Bool
matchesTrafficDirection dir destination =
    case destination of
        TwoLaneRoad (Curve TopRight) OneWay ->
            dir == Up

        TwoLaneRoad (Curve TopLeft) OneWay ->
            dir == Left

        TwoLaneRoad (Curve BottomRight) OneWay ->
            dir == Right

        TwoLaneRoad (Curve BottomLeft) OneWay ->
            dir == Down

        TwoLaneRoad _ OneWay ->
            dir == Right || dir == Up

        _ ->
            True


connected : Direction -> Tile -> Tile -> Bool
connected trafficDirection origin destination =
    let
        originConnections =
            potentialConnections origin

        destinationConnections =
            potentialConnections destination

        connects dir =
            dir == trafficDirection && List.member (Direction.opposite dir) destinationConnections
    in
    matchesTrafficDirection trafficDirection destination
        && List.any connects originConnections


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


defaultIntersectionControl : IntersectionShape -> IntersectionControl
defaultIntersectionControl shape =
    case shape of
        T dir ->
            Direction.toOrientation dir
                |> Direction.oppositeOrientation
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


toString : Tile -> String
toString tile =
    case tile of
        TwoLaneRoad _ Both ->
            "Two-way road"

        TwoLaneRoad _ OneWay ->
            "One-way road"

        Intersection _ _ ->
            "Intersection"
