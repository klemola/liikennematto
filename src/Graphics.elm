module Graphics exposing
    ( buildingAsset
    , carAsset
    , grid
    , renderedSizeFromUnits
    , texture
    , tileAsset
    )

import Board exposing (Tile)
import Car exposing (Car, CarKind(..))
import Cell exposing (Corner(..), OrthogonalDirection(..))
import Collage exposing (Collage, image)
import Collage.Layout as Layout
import Lot exposing (BuildingKind(..))


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


texture : ( Float, Float ) -> String -> Collage msg
texture size asset =
    image size ("assets/" ++ asset)


renderedSizeFromUnits : ( Int, Int ) -> Float -> ( Float, Float )
renderedSizeFromUnits ( x, y ) multiplier =
    ( toFloat x * multiplier, toFloat y * multiplier )


tileAsset : Tile -> String
tileAsset tile =
    case tile of
        0 ->
            "road_2_lanes_horizontal.png"

        1 ->
            "road_2_lanes_deadend_down.png"

        2 ->
            "road_2_lanes_deadend_right.png"

        3 ->
            "road_2_lanes_curve_bottom_right.png"

        4 ->
            "road_2_lanes_deadend_left.png"

        5 ->
            "road_2_lanes_curve_bottom_left.png"

        6 ->
            "road_2_lanes_horizontal.png"

        7 ->
            "intersection_2_lanes_t_up.png"

        8 ->
            "road_2_lanes_deadend_up.png"

        9 ->
            "road_2_lanes_vertical.png"

        10 ->
            "road_2_lanes_curve_top_right.png"

        11 ->
            "intersection_2_lanes_t_left.png"

        12 ->
            "road_2_lanes_curve_top_left.png"

        13 ->
            "intersection_2_lanes_t_right.png"

        14 ->
            "intersection_2_lanes_t_down.png"

        15 ->
            "intersection_2_lanes_x.png"

        _ ->
            "road_not_found.png"


carAsset : Car -> String
carAsset car =
    case car.kind of
        SedanA ->
            "car_blue_1.png"

        SedanB ->
            "car_red_1.png"

        SedanC ->
            "car_yellow_1.png"

        SedanD ->
            "car_green_1.png"

        SedanE ->
            "car_black_1.png"

        TestCar ->
            "car_white_1.png"


buildingAsset : BuildingKind -> String
buildingAsset kind =
    case kind of
        ResidentialA ->
            "residential_a.png"

        ResidentialB ->
            "residential_b.png"

        ResidentialC ->
            "residential_c.png"

        ResidentialD ->
            "residential_d.png"

        ResidentialE ->
            "residential_e.png"

        TwoByOneTest ->
            "geometry_test_2x1.png"

        ThreeByThreeTest ->
            "geometry_test_3x3.png"

        TwoByThreeTest ->
            "geometry_test_2x3.png"
