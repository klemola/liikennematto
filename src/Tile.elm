module Tile exposing
    ( CurveKind(..)
    , IntersectionControl(..)
    , IntersectionShape(..)
    , RoadKind(..)
    , Tile(..)
    , isIntersection
    , isRoad
    , potentialConnections
    , priorityDirections
    , toggleIntersectionControl
    , trafficLightsAllowEntry
    , validNeighbors
    )

import Direction exposing (Direction(..), Orientation(..))
import TrafficLight exposing (TrafficLight)


type RoadKind
    = Regular Orientation
    | Curve CurveKind
    | Deadend Direction


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


type alias TrafficLights =
    List TrafficLight


type Tile
    = TwoLaneRoad RoadKind
    | Intersection IntersectionControl IntersectionShape
    | Terrain


trafficLightsAllowEntry : TrafficLights -> Direction -> Bool
trafficLightsAllowEntry trafficLights entryDirection =
    let
        signalXsEntryAllowed tl =
            TrafficLight.isGreen tl && tl.facing == entryDirection
    in
    List.any signalXsEntryAllowed trafficLights


isRoad : Tile -> Bool
isRoad tile =
    case tile of
        TwoLaneRoad _ ->
            True

        _ ->
            False


isRegularRoad : Tile -> Bool
isRegularRoad tile =
    case tile of
        TwoLaneRoad (Regular _) ->
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
        TwoLaneRoad (Regular orientation) ->
            Direction.fromOrientation orientation

        TwoLaneRoad (Curve TopRight) ->
            [ Left, Down ]

        TwoLaneRoad (Curve TopLeft) ->
            [ Right, Down ]

        TwoLaneRoad (Curve BottomRight) ->
            [ Left, Up ]

        TwoLaneRoad (Curve BottomLeft) ->
            [ Right, Up ]

        TwoLaneRoad (Deadend dir) ->
            [ Direction.opposite dir ]

        Intersection _ Crossroads ->
            Direction.all

        Intersection _ (T dir) ->
            dir :: Direction.cross dir

        Terrain ->
            []


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
