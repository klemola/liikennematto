module Graphics exposing (carAsset, grid, intersectionAsset, marker, roadAsset, texture)

import Car exposing (Car, CarKind(..))
import Collage exposing (Collage, image, invisible, shift, square, styled, transparent)
import Collage.Layout as Layout
import Direction exposing (Direction(..), Orientation(..))
import Tile exposing (CurveKind(..), IntersectionShape(..), RoadKind(..))


grid : Int -> (Int -> Int -> Collage msg) -> Collage msg
grid size getCollage =
    let
        rg =
            List.range 1 size

        col x =
            rg
                |> List.map (getCollage x)
                |> Layout.vertical

        rows =
            rg
                |> List.map col
    in
    Layout.horizontal rows


marker : Float -> Float -> Direction -> Collage msg -> Collage msg
marker tileSize offset side presentation =
    let
        ( anchor, shiftAmount ) =
            case side of
                Up ->
                    ( Layout.top, ( 0, -offset ) )

                Right ->
                    ( Layout.right, ( -offset, 0 ) )

                Down ->
                    ( Layout.bottom, ( 0, offset ) )

                Left ->
                    ( Layout.left, ( offset, 0 ) )

        boundaries =
            square tileSize
                |> styled ( transparent, invisible )

        positionedPresentation =
            presentation
                |> shift shiftAmount
                |> Layout.at anchor
    in
    boundaries
        |> positionedPresentation


texture : Float -> String -> Collage msg
texture size asset =
    image ( size, size ) ("assets/" ++ asset)


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


carAsset : Car -> String
carAsset car =
    case car.kind of
        Sedan1 ->
            "car_blue_1.png"

        Sedan2 ->
            "car_red_1.png"

        Sedan3 ->
            "car_green_1.png"

        Sedan4 ->
            "car_yellow_1.png"

        Sedan5 ->
            "car_black_1.png"
