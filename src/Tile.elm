module Tile exposing (..)

import Collage exposing (Collage, square, styled, uniform)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..))
import Graphics exposing (texture)
import TrafficLight exposing (TrafficLight)


type RoadKind
    = Horizontal
    | Vertical
    | Curve CurveKind
    | Deadend Direction


type CurveKind
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


type IntersectionControl
    = Signal TrafficLights
    | Yield
    | Stop
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
    | Empty


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


update : Tile -> Tile
update tile =
    case tile of
        Intersection (Signal trafficLights) shape ->
            let
                next =
                    trafficLights
                        |> List.map TrafficLight.update
                        |> Signal
            in
            Intersection next shape

        _ ->
            tile


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

        Intersection Yield shape ->
            signs tileSize "yield_sign.png" shape
                |> intersection shape

        Intersection Stop shape ->
            signs tileSize "stop_sign.png" shape
                |> intersection shape

        Intersection Uncontrolled shape ->
            intersection shape []

        Terrain ->
            ground (Color.rgb255 33 191 154)

        Empty ->
            ground Color.yellow


signs : Float -> String -> IntersectionShape -> List (Collage msg)
signs tileSize asset intersectionShape =
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
                    Direction.horizontal

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

        T Down ->
            "intersection_2_lanes_t_down.png"

        T Right ->
            "intersection_2_lanes_t_right.png"

        T Left ->
            "intersection_2_lanes_t_left.png"

        Crossroads ->
            "intersection_2_lanes_x.png"


roadAsset : RoadKind -> String
roadAsset kind =
    case kind of
        Horizontal ->
            "road_2_lanes_horizontal.png"

        Vertical ->
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

        Deadend Down ->
            "road_2_lanes_deadend_down.png"

        Deadend Left ->
            "road_2_lanes_deadend_left.png"

        Deadend Right ->
            "road_2_lanes_deadend_right.png"
