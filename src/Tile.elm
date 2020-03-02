module Tile exposing (..)

import Car exposing (Cars)
import Collage exposing (..)
import Collage.Layout as Layout
import Color
import Coords exposing (Coords)
import Direction exposing (Direction(..))
import TrafficLight exposing (TrafficLights)


type Tile
    = TwoLaneRoad Cars
    | SignalControlledIntersection Cars TrafficLights
    | YieldControlledIntersection Cars
    | StopControlledIntersection Cars
    | UncontrolledIntersection Cars
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

        SignalControlledIntersection cars _ ->
            cars

        YieldControlledIntersection cars ->
            cars

        _ ->
            []


setCars : Tile -> Cars -> Tile
setCars tile cars =
    case tile of
        TwoLaneRoad _ ->
            TwoLaneRoad cars

        SignalControlledIntersection _ trafficLights ->
            SignalControlledIntersection cars trafficLights

        YieldControlledIntersection _ ->
            YieldControlledIntersection cars

        StopControlledIntersection _ ->
            StopControlledIntersection cars

        UncontrolledIntersection _ ->
            UncontrolledIntersection cars

        _ ->
            tile


hasCars : Tile -> Bool
hasCars tile =
    List.length (getCars tile) > 0


canEnter : Tile -> Coords -> List ( Coords, Tile ) -> Direction -> Bool
canEnter tile coords neighbors entryDirection =
    let
        -- TODO: take in account car status (no need to wait until the next car is moving)
        noCollision car =
            car.direction /= entryDirection

        verticalTrafficInXs carsInXs =
            neighbors
                |> List.filterMap
                    (\( c, t ) ->
                        if Tuple.second coords - Tuple.second c /= 0 then
                            Just (getCars t)

                        else
                            Nothing
                    )
                |> List.concat
                |> List.append carsInXs

        signalXsEntryAllowed tl =
            TrafficLight.isGreen tl && tl.facing == entryDirection

        yieldXsEntryAllowed carsInXs =
            if List.member entryDirection Direction.horizontal then
                List.length (verticalTrafficInXs carsInXs) == 0

            else
                True
    in
    case tile of
        TwoLaneRoad cars ->
            List.all noCollision cars

        SignalControlledIntersection _ trafficLights ->
            List.any signalXsEntryAllowed trafficLights

        YieldControlledIntersection cars ->
            yieldXsEntryAllowed cars

        StopControlledIntersection cars ->
            True

        UncontrolledIntersection cars ->
            True

        _ ->
            False


update : Tile -> Tile
update tile =
    case tile of
        SignalControlledIntersection cars trafficLights ->
            trafficLights
                |> List.map TrafficLight.update
                |> SignalControlledIntersection cars

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

        SignalControlledIntersection cars trafficLights ->
            Layout.stack (carsInTile cars ++ trafficLightsInTile trafficLights ++ [ ground Color.darkGray ])

        YieldControlledIntersection cars ->
            Layout.stack (carsInTile cars ++ [ ground Color.lightYellow ])

        StopControlledIntersection cars ->
            Layout.stack (carsInTile cars ++ [ ground Color.lightRed ])

        UncontrolledIntersection cars ->
            Layout.stack (carsInTile cars ++ [ ground Color.darkGray ])

        Terrain ->
            ground (Color.rgb255 102 153 80)

        Empty ->
            ground Color.yellow
