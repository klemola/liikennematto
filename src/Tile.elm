module Tile exposing (..)

import Car exposing (Cars)
import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color exposing (black, darkGray, lightGreen, yellow)


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , timeRemaining : Int
    }


type alias TrafficLights =
    List TrafficLight


type Tile
    = TwoLaneRoad Cars
    | Intersection Cars TrafficLights
    | Terrain
    | Empty


blockSize : Float
blockSize =
    64


hasCars : Tile -> Bool
hasCars tile =
    case tile of
        TwoLaneRoad cars ->
            List.length cars > 0

        Intersection cars _ ->
            List.length cars > 0

        _ ->
            False


setCars : Tile -> Cars -> Tile
setCars tile cars =
    case tile of
        TwoLaneRoad _ ->
            TwoLaneRoad cars

        Intersection _ trafficLights ->
            Intersection cars trafficLights

        _ ->
            tile


view : Tile -> Collage msg
view tile =
    let
        carsInTile cars =
            List.map (Car.view blockSize) cars

        border =
            solid thin <| uniform black

        ground color =
            rectangle blockSize blockSize
                |> styled ( uniform color, border )
    in
    case tile of
        TwoLaneRoad cars ->
            stack (carsInTile cars ++ [ ground darkGray ])

        Intersection cars trafficLights ->
            stack (carsInTile cars ++ [ ground darkGray ])

        Terrain ->
            ground lightGreen

        Empty ->
            ground yellow
