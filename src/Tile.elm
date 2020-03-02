module Tile exposing (..)

import Collage exposing (..)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..))
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


defaultBorder : LineStyle
defaultBorder =
    solid ultrathin <| uniform Color.darkCharcoal


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
        trafficLightsInTile tls =
            List.map (TrafficLight.view tileSize) tls

        ground color =
            square tileSize
                |> styled ( uniform color, defaultBorder )
    in
    case tile of
        TwoLaneRoad ->
            ground Color.darkGray

        SignalControlledIntersection trafficLights ->
            Layout.stack (trafficLightsInTile trafficLights ++ [ ground Color.darkGray ])

        YieldControlledIntersection ->
            ground Color.lightYellow

        StopControlledIntersection ->
            ground Color.lightRed

        UncontrolledIntersection ->
            ground Color.darkGray

        Terrain ->
            ground (Color.rgb255 102 153 80)

        Empty ->
            ground Color.yellow
