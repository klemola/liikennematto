module Tile exposing (..)

import Collage exposing (Collage, square, styled, uniform)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..))
import Graphics
import TrafficLight exposing (TrafficLight)


type alias TrafficLights =
    List TrafficLight


type Tile
    = TwoLaneRoad
    | SignalControlledIntersection TrafficLights
    | YieldControlledIntersection
    | StopControlledIntersection
    | UncontrolledIntersection
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
        TwoLaneRoad ->
            True

        _ ->
            False


update : Tile -> Tile
update tile =
    case tile of
        SignalControlledIntersection trafficLights ->
            trafficLights
                |> List.map TrafficLight.update
                |> SignalControlledIntersection

        _ ->
            tile


view : Float -> Tile -> Collage msg
view tileSize tile =
    let
        ground color =
            square tileSize
                |> styled ( uniform color, Graphics.defaultBorderStyle )

        roadWithMarkers content =
            Layout.stack (content ++ [ ground Color.darkGray ])

        roadWithTrafficSigns color =
            -- Once intersections have a type (e.g. T or four-way traffic), update the direction list
            Direction.horizontal
                |> List.map (Graphics.border tileSize color)
                |> roadWithMarkers
    in
    case tile of
        TwoLaneRoad ->
            ground Color.darkGray

        SignalControlledIntersection trafficLights ->
            trafficLights
                |> List.map (TrafficLight.view tileSize)
                |> roadWithMarkers

        YieldControlledIntersection ->
            roadWithTrafficSigns Color.lightYellow

        StopControlledIntersection ->
            roadWithTrafficSigns Color.lightRed

        UncontrolledIntersection ->
            ground Color.darkGray

        Terrain ->
            ground (Color.rgb255 102 153 80)

        Empty ->
            ground Color.yellow
