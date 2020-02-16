module Tile exposing (..)

import Car exposing (Cars)
import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad Cars
    | Intersection Cars TrafficLights
    | Terrain
    | Empty


blockSize : Float
blockSize =
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


canEnter : Tile -> Bool
canEnter tile =
    case tile of
        TwoLaneRoad _ ->
            True

        Intersection _ trafficLights ->
            List.all TrafficLight.isGreen trafficLights

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
            List.map (Car.view blockSize) cars

        trafficLightsInTile tls =
            List.map (TrafficLight.view blockSize) tls

        ground color =
            rectangle blockSize blockSize
                |> styled ( uniform color, defaultBorder )
    in
    case tile of
        TwoLaneRoad cars ->
            stack (carsInTile cars ++ [ ground Color.darkGray ])

        Intersection cars trafficLights ->
            stack (carsInTile cars ++ trafficLightsInTile trafficLights ++ [ ground Color.darkGray ])

        Terrain ->
            ground Color.darkGreen

        Empty ->
            ground Color.yellow
