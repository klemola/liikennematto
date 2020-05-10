module Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..), intersectionAsset, isIntersection, isRoad, potentialConnections, priorityDirections, roadAsset, trafficLightsAllowEntry, view)

import Collage exposing (Collage, square, styled, uniform)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..), Orientation(..))
import Graphics exposing (texture)
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
            Direction.other dir

        Terrain ->
            []


view : Float -> Tile -> Collage msg
view tileSize tile =
    let
        ground color =
            square tileSize
                |> styled ( uniform color, Collage.invisible )

        intersection shape content =
            Layout.stack (content ++ [ texture tileSize (intersectionAsset shape) ])
    in
    case tile of
        TwoLaneRoad kind ->
            roadAsset kind
                |> texture tileSize

        Intersection (Signal trafficLights) shape ->
            trafficLights
                |> List.map (TrafficLight.view tileSize)
                |> intersection shape

        Intersection (Yield orientation) shape ->
            signs tileSize orientation shape "yield_sign.png"
                |> intersection shape

        Intersection (Stop orientation) shape ->
            signs tileSize orientation shape "stop_sign.png"
                |> intersection shape

        Intersection Uncontrolled shape ->
            intersection shape []

        Terrain ->
            ground (Color.rgb255 33 191 154)


signs : Float -> Orientation -> IntersectionShape -> String -> List (Collage msg)
signs tileSize orientation intersectionShape asset =
    let
        size =
            tileSize * 0.25

        offset =
            size * 0.5

        locations =
            case intersectionShape of
                T dir ->
                    [ dir ]

                Crossroads ->
                    Direction.fromOrientation orientation

        presentation dir =
            Graphics.texture size asset
                |> Graphics.marker tileSize offset dir
    in
    locations
        |> List.map presentation


intersectionAsset : IntersectionShape -> String
intersectionAsset shape =
    case shape of
        T Up ->
            "intersection_2_lanes_t_up.png"

        T Right ->
            "intersection_2_lanes_t_right.png"

        T Down ->
            "intersection_2_lanes_t_down.png"

        T Left ->
            "intersection_2_lanes_t_left.png"

        Crossroads ->
            "intersection_2_lanes_x.png"


roadAsset : RoadKind -> String
roadAsset kind =
    case kind of
        Regular Horizontal ->
            "road_2_lanes_horizontal.png"

        Regular Vertical ->
            "road_2_lanes_vertical.png"

        Curve TopRight ->
            "road_2_lanes_curve_top_right.png"

        Curve TopLeft ->
            "road_2_lanes_curve_top_left.png"

        Curve BottomRight ->
            "road_2_lanes_curve_bottom_right.png"

        Curve BottomLeft ->
            "road_2_lanes_curve_bottom_left.png"

        Deadend Up ->
            "road_2_lanes_deadend_up.png"

        Deadend Right ->
            "road_2_lanes_deadend_right.png"

        Deadend Down ->
            "road_2_lanes_deadend_down.png"

        Deadend Left ->
            "road_2_lanes_deadend_left.png"
