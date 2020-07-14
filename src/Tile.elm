module Tile exposing
    ( CurveKind(..)
    , IntersectionControl(..)
    , IntersectionShape(..)
    , RoadKind(..)
    , Tile(..)
    , TrafficDirection(..)
    , connected
    , isIntersection
    , isRoad
    , potentialConnections
    , priorityDirections
    , toggleIntersectionControl
    , toggleTrafficDirection
    , validNeighbors
    )

import Direction exposing (Direction(..), Orientation(..))
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad RoadKind TrafficDirection
    | Intersection IntersectionControl IntersectionShape
    | Terrain


type RoadKind
    = Regular Orientation
    | Curve CurveKind
    | Deadend Direction


type TrafficDirection
    = Both
    | OneWay


type CurveKind
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


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


isIntersection : Tile -> Bool
isIntersection tile =
    case tile of
        Intersection _ _ ->
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

        Terrain ->
            []


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


validNeighbors : Tile -> Tile -> Bool
validNeighbors tileA tileB =
    -- this pratically prevents placing intersections right next to each other
    -- Room for improvement: It's possible to check if # of combined "potential connections"
    -- ...is bigger than the # of individual tiles == complexity will increase, and prevent it
    isRoad tileA || isRoad tileB


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


toggleTrafficDirection : Tile -> Tile
toggleTrafficDirection tile =
    case tile of
        TwoLaneRoad kind OneWay ->
            TwoLaneRoad kind Both

        TwoLaneRoad kind Both ->
            TwoLaneRoad kind OneWay

        _ ->
            tile
