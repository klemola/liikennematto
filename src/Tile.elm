module Tile exposing (..)

import Car exposing (Cars)
import Collage exposing (..)
import Collage.Layout exposing (stack)
import Color exposing (Color)


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


defaultBorder : LineStyle
defaultBorder =
    solid thin <| uniform Color.black


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


defaultTrafficLight : TrafficLight
defaultTrafficLight =
    makeTrafficLight Green


makeTrafficLight : TrafficLightKind -> TrafficLight
makeTrafficLight kind =
    case kind of
        Green ->
            TrafficLight Green 5

        Yellow ->
            TrafficLight Yellow 3

        Red ->
            TrafficLight Red 3


advanceTrafficLights : Tile -> Tile
advanceTrafficLights tile =
    let
        advanceTimer tl =
            { tl | timeRemaining = tl.timeRemaining - 1 }

        nextLight tl =
            if tl.timeRemaining == 0 then
                advanceTrafficLightCycle tl.kind
                    |> makeTrafficLight

            else
                tl
    in
    case tile of
        Intersection cars trafficLights ->
            trafficLights
                |> List.map advanceTimer
                |> List.map nextLight
                |> Intersection cars

        _ ->
            tile


advanceTrafficLightCycle : TrafficLightKind -> TrafficLightKind
advanceTrafficLightCycle tlKind =
    case tlKind of
        Green ->
            Red

        Yellow ->
            Green

        Red ->
            Yellow


view : Tile -> Collage msg
view tile =
    let
        carsInTile cars =
            List.map (Car.view blockSize) cars

        ground color border =
            rectangle blockSize blockSize
                |> styled ( uniform color, border )

        tlBorder tls =
            case List.head tls of
                Just tl ->
                    solid thin <| uniform (trafficLightToVisualColor tl.kind)

                Nothing ->
                    defaultBorder
    in
    case tile of
        TwoLaneRoad cars ->
            stack (carsInTile cars ++ [ ground Color.darkGray defaultBorder ])

        Intersection cars trafficLights ->
            stack (carsInTile cars ++ [ ground Color.darkGray (tlBorder trafficLights) ])

        Terrain ->
            ground Color.lightGreen defaultBorder

        Empty ->
            ground Color.yellow defaultBorder


trafficLightToVisualColor : TrafficLightKind -> Color
trafficLightToVisualColor tlKind =
    case tlKind of
        Green ->
            Color.green

        Yellow ->
            Color.yellow

        Red ->
            Color.red
