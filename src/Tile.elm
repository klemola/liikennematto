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
    , isTerrain
    , potentialConnections
    , priorityDirections
    , toggleIntersectionControl
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


isRegularRoad : Tile -> Bool
isRegularRoad tile =
    case tile of
        TwoLaneRoad (Regular _) _ ->
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


isTerrain : Tile -> Bool
isTerrain tile =
    tile == Terrain


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


matchesTrafficDirection : Tile -> Direction -> Bool
matchesTrafficDirection tile toDir =
    case tile of
        -- traffic from either Left or Up (lowest x or y)
        TwoLaneRoad _ OneWay ->
            toDir == Left || toDir == Up

        _ ->
            True


connected : Direction -> Tile -> Tile -> Bool
connected fromDir origin destination =
    let
        originConnections =
            potentialConnections origin
                |> List.filter (matchesTrafficDirection origin)

        destinationConnections =
            potentialConnections destination

        connects dir =
            dir == fromDir && List.member (Direction.opposite dir) destinationConnections
    in
    List.any connects originConnections


validNeighbors : Tile -> Tile -> Bool
validNeighbors tileA tileB =
    List.any isRegularRoad [ tileA, tileB ]


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
