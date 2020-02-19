module Tile exposing (..)

import Car exposing (Cars)
import Collage exposing (..)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..))
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad Cars
    | Intersection Cars TrafficLights
    | Terrain
    | Empty


tileSize : Float
tileSize =
    64


defaultBorder : LineStyle
defaultBorder =
    solid ultrathin <| uniform Color.darkCharcoal


getCars : Tile -> Cars
getCars tile =
    case tile of
        TwoLaneRoad cars ->
            cars

        Intersection cars _ ->
            cars

        _ ->
            []


setCars : Tile -> Cars -> Tile
setCars tile cars =
    case tile of
        TwoLaneRoad _ ->
            TwoLaneRoad cars

        Intersection _ trafficLights ->
            Intersection cars trafficLights

        _ ->
            tile


hasCars : Tile -> Bool
hasCars tile =
    List.length (getCars tile) > 0


canEnter : Tile -> Direction -> Bool
canEnter tile entryDirection =
    let
        noCollision car =
            car.direction /= entryDirection

        entryAllowed tl =
            TrafficLight.isGreen tl && tl.facing == entryDirection
    in
    case tile of
        TwoLaneRoad cars ->
            List.all noCollision cars

        Intersection _ trafficLights ->
            List.any entryAllowed trafficLights

        _ ->
            False


advanceTrafficLights : Tile -> Tile
advanceTrafficLights tile =
    case tile of
        Intersection cars trafficLights ->
            trafficLights
                |> List.map TrafficLight.next
                |> Intersection cars

        _ ->
            tile


view : Tile -> Collage msg
view tile =
    let
        carsInTile cars =
            List.map (Car.view tileSize) cars

        trafficLightsInTile tls =
            List.map (TrafficLight.view tileSize) tls

        ground color =
            square tileSize
                |> styled ( uniform color, defaultBorder )
    in
    case tile of
        TwoLaneRoad cars ->
            Layout.stack (carsInTile cars ++ [ ground Color.darkGray ])

        Intersection cars trafficLights ->
            Layout.stack (carsInTile cars ++ trafficLightsInTile trafficLights ++ [ ground Color.darkGray ])

        Terrain ->
            ground (Color.rgb255 102 153 80)

        Empty ->
            ground Color.yellow
