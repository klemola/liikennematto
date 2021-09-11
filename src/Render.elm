module Render exposing (view)

import Angle
import Board exposing (Board, Tile)
import Car exposing (Car, CarKind(..), Cars)
import Cell exposing (Cell)
import Color
import Config
    exposing
        ( boardSizeScaled
        , pixelsToMetersRatio
        , tileSize
        )
import Dict
import Geometry exposing (LMPoint2d)
import Graph exposing (Node)
import Html exposing (Html)
import Length exposing (Length)
import Lot exposing (BuildingKind(..), Lot, Lots)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d
import Polygon2d
import Quantity exposing (Quantity)
import RoadNetwork exposing (ConnectionKind(..), RoadNetwork, TrafficControl(..))
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Svg.Keyed
import Svg.Lazy
import TrafficLight exposing (TrafficLight, TrafficLightColor(..), TrafficLights)
import Triangle2d
import World exposing (World)


type alias DebugLayers =
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }


nodeSize : Quantity Float Pixels
nodeSize =
    Pixels.float 4


signSize : Quantity Float Pixels
signSize =
    Pixels.float 10


trafficLightRadius : Quantity Float Pixels
trafficLightRadius =
    signSize |> Quantity.divideBy 2


carWidthPixels : Float
carWidthPixels =
    toPixelsValue Car.width


carLengthPixels : Float
carLengthPixels =
    toPixelsValue Car.length


boardSizeScaledPixels : Float
boardSizeScaledPixels =
    boardSizeScaled |> Pixels.inPixels |> toFloat


boardSizeScaledStr : String
boardSizeScaledStr =
    String.fromFloat boardSizeScaledPixels


tileSizePixels : Float
tileSizePixels =
    tileSize |> Pixels.inPixels


view : World -> DebugLayers -> Html msg
view { board, cars, lots, roadNetwork, trafficLights } debugLayers =
    Svg.svg
        [ Attributes.width boardSizeScaledStr
        , Attributes.height boardSizeScaledStr
        , Attributes.viewBox <| "0 0 " ++ boardSizeScaledStr ++ " " ++ boardSizeScaledStr
        ]
        ([ Svg.Lazy.lazy renderBoard board
         , Svg.Lazy.lazy renderLots lots
         , renderCars cars
         , Svg.Lazy.lazy renderTrafficLights trafficLights
         , renderTrafficSigns roadNetwork
         ]
            ++ renderDebugLayers debugLayers cars roadNetwork
        )


renderColors : { road : Color.Color, terrain : Color.Color, sidewalk : Color.Color, sidewalkEdge : Color.Color }
renderColors =
    { road = Color.rgb255 52 65 67
    , terrain = Color.rgb255 33 191 154
    , sidewalk = Color.rgb255 191 213 217
    , sidewalkEdge = Color.rgb255 44 56 58
    }


renderBoard : Board -> Svg msg
renderBoard board =
    board
        |> Dict.foldl
            (\cell tile acc -> ( Cell.toString cell, renderTile cell tile ) :: acc)
            []
        |> Svg.Keyed.node "g" []


renderTile : Cell -> Tile -> Svg msg
renderTile cell tile =
    let
        asset =
            "assets/" ++ tileAsset tile

        { x, y } =
            Cell.bottomLeftCorner cell |> pointToPixels
    in
    Svg.image
        [ Attributes.xlinkHref asset
        , Attributes.x (String.fromFloat x)
        , Attributes.y (String.fromFloat (boardSizeScaledPixels - tileSizePixels - y))
        , Attributes.width (String.fromFloat tileSizePixels)
        , Attributes.height (String.fromFloat tileSizePixels)
        ]
        []


renderCars : Cars -> Svg msg
renderCars cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "Car-" ++ String.fromInt car.id, renderCar car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCar : Car -> Svg msg
renderCar car =
    let
        asset =
            "assets/" ++ carAsset car

        { x, y } =
            pointToPixels car.position

        renderX =
            x - (carLengthPixels / 2)

        renderY =
            boardSizeScaledPixels - (carWidthPixels / 2) - y

        rotateVal =
            car.orientation
                |> Quantity.negate
                |> Angle.inDegrees
                |> String.fromFloat

        rotateStr =
            "rotate(" ++ rotateVal ++ "," ++ String.fromFloat x ++ "," ++ String.fromFloat (boardSizeScaledPixels - y) ++ ")"
    in
    Svg.image
        [ Attributes.xlinkHref asset
        , Attributes.x <| String.fromFloat renderX
        , Attributes.y <| String.fromFloat renderY
        , Attributes.transform rotateStr
        , Attributes.width <| String.fromFloat carLengthPixels
        , Attributes.height <| String.fromFloat carWidthPixels
        ]
        []


renderLots : Lots -> Svg msg
renderLots lots =
    lots
        |> Dict.foldl (\_ lot acc -> ( buildingAsset lot.content.kind, renderLot lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : Lot -> Svg msg
renderLot lot =
    let
        { x, y } =
            pointToPixels lot.position

        asset =
            "assets/" ++ buildingAsset lot.content.kind

        width =
            toPixelsValue lot.width

        height =
            toPixelsValue lot.height

        mask =
            sidewalkMask lot
    in
    Svg.g []
        [ Svg.image
            [ Attributes.xlinkHref asset
            , Attributes.x <| String.fromFloat x
            , Attributes.y <| String.fromFloat <| boardSizeScaledPixels - height - y
            , Attributes.width <| String.fromFloat width
            , Attributes.height <| String.fromFloat height
            ]
            []
        , mask
        ]


sidewalkMask : Lot -> Svg msg
sidewalkMask lot =
    -- sidewalk mask hides terrain between sidewalk and the lot
    -- Room for improvement: use special road tiles when connected to a lot
    let
        lotPosition =
            pointToPixels lot.position

        { x, y } =
            pointToPixels lot.entryDetails.entryPoint

        width =
            toPixelsValue lot.entryDetails.width

        height =
            toPixelsValue lot.entryDetails.height
    in
    Svg.rect
        [ Attributes.width <| String.fromFloat width
        , Attributes.height <| String.fromFloat height
        , Attributes.fill <| Color.toCssString <| renderColors.sidewalk
        , Attributes.x <| String.fromFloat <| x + lotPosition.x - (width / 2)
        , Attributes.y <| String.fromFloat <| boardSizeScaledPixels - lotPosition.y - y - (height / 2)
        ]
        []


renderTrafficLights : TrafficLights -> Svg msg
renderTrafficLights trafficLights =
    trafficLights
        |> Dict.foldl (\_ tl acc -> ( "TrafficLight-" ++ String.fromInt tl.id, renderTrafficLight tl ) :: acc) []
        |> Svg.Keyed.node "g" []


renderTrafficLight : TrafficLight -> Svg msg
renderTrafficLight trafficLight =
    let
        { x, y } =
            pointToPixels trafficLight.position

        radius =
            Pixels.inPixels trafficLightRadius

        color =
            case trafficLight.color of
                Green ->
                    Color.darkGreen

                Yellow ->
                    Color.darkYellow

                Red ->
                    Color.darkRed
    in
    Svg.circle
        [ Attributes.r <| String.fromFloat radius
        , Attributes.cx <| String.fromFloat x
        , Attributes.cy <| String.fromFloat (boardSizeScaledPixels - y)
        , Attributes.fill <| Color.toCssString color
        , Attributes.stroke <| Color.toCssString Color.grey
        , Attributes.strokeWidth <| String.fromInt 1
        ]
        []


renderTrafficSigns : RoadNetwork -> Svg msg
renderTrafficSigns roadNetwork =
    roadNetwork
        |> Graph.nodes
        |> List.filterMap
            (\node ->
                case node.label.trafficControl of
                    Yield ->
                        Just
                            ( "Yield-" ++ String.fromInt node.id
                            , renderYieldSign node
                            )

                    _ ->
                        Nothing
            )
        |> Svg.Keyed.node "g" []


renderYieldSign : Node RoadNetwork.Connection -> Svg msg
renderYieldSign node =
    let
        size =
            Pixels.inPixels signSize

        { x, y } =
            pointToPixels node.label.position

        asset =
            "assets/yield_sign.png"
    in
    Svg.image
        [ Attributes.xlinkHref asset
        , Attributes.x <| String.fromFloat (x - size / 2)
        , Attributes.y <| String.fromFloat <| boardSizeScaledPixels - y - (size / 2)
        , Attributes.width <| String.fromFloat size
        , Attributes.height <| String.fromFloat size
        ]
        []



--
-- Debug
--


renderDebugLayers : DebugLayers -> Cars -> RoadNetwork -> List (Svg msg)
renderDebugLayers { showRoadNetwork, showCarDebugVisuals } cars roadNetwork =
    let
        carsLayer =
            if showCarDebugVisuals then
                Just (renderCarsDebug cars)

            else
                Nothing

        roadNetworkLayer =
            if showRoadNetwork then
                Just (renderRoadNetwork roadNetwork)

            else
                Nothing
    in
    Maybe.values
        [ carsLayer
        , roadNetworkLayer
        ]


renderRoadNetwork : RoadNetwork -> Svg msg
renderRoadNetwork roadNetwork =
    let
        nodeColor kind =
            case kind of
                LaneStart ->
                    Color.white

                LaneEnd ->
                    Color.lightBlue

                DeadendEntry ->
                    Color.lightPurple

                DeadendExit ->
                    Color.darkGray

                LotEntry ->
                    Color.lightGreen

                Stopgap ->
                    Color.lightYellow

        nodes =
            roadNetwork
                |> Graph.fold
                    (\nodeCtx acc ->
                        let
                            radius =
                                Pixels.inPixels nodeSize

                            { position, kind } =
                                nodeCtx.node.label

                            { x, y } =
                                pointToPixels position
                        in
                        ( "Node-" ++ String.fromInt nodeCtx.node.id
                        , Svg.circle
                            [ Attributes.r <| String.fromFloat radius
                            , Attributes.cx <| String.fromFloat x
                            , Attributes.cy <| String.fromFloat (boardSizeScaledPixels - y)
                            , Attributes.fill <| Color.toCssString <| nodeColor kind
                            ]
                            []
                        )
                            :: acc
                    )
                    []
                |> Svg.Keyed.node "g" []

        edges =
            roadNetwork
                |> Graph.edges
                |> List.filterMap
                    (\edge ->
                        let
                            fromNode =
                                Graph.get edge.from roadNetwork

                            toNode =
                                Graph.get edge.to roadNetwork
                        in
                        Maybe.map2
                            (\fromNodeCtx toNodeCtx ->
                                let
                                    from =
                                        pointToPixels fromNodeCtx.node.label.position

                                    to =
                                        pointToPixels toNodeCtx.node.label.position

                                    fromStr =
                                        String.fromFloat from.x ++ " " ++ String.fromFloat (boardSizeScaledPixels - from.y)

                                    toStr =
                                        String.fromFloat to.x ++ " " ++ String.fromFloat (boardSizeScaledPixels - to.y)
                                in
                                ( "Edge-" ++ String.fromInt fromNodeCtx.node.id ++ String.fromInt toNodeCtx.node.id
                                , Svg.path
                                    [ Attributes.stroke <| Color.toCssString Color.orange
                                    , Attributes.strokeWidth <| "2"
                                    , Attributes.d <| "M " ++ fromStr ++ " L " ++ toStr
                                    ]
                                    []
                                )
                            )
                            fromNode
                            toNode
                    )
                |> Svg.Keyed.node "g" []
    in
    Svg.g []
        [ edges
        , nodes
        ]


renderCarsDebug : Cars -> Svg msg
renderCarsDebug cars =
    cars
        |> Dict.foldl
            (\_ car acc ->
                ( "CarDebug-" ++ String.fromInt car.id, renderCarDebug car ) :: acc
            )
            []
        |> Svg.Keyed.node "g" []


renderCarDebug : Car -> Svg msg
renderCarDebug car =
    Svg.g []
        [ renderCarFieldOfView car
        , renderCarCollisionDetection car
        , renderCarPath car
        ]


renderCarPath : Car -> Svg msg
renderCarPath car =
    let
        points =
            toPointsString car.localPath
    in
    Svg.polyline
        [ Attributes.stroke <| Color.toCssString Color.red
        , Attributes.strokeWidth <| "2"
        , Attributes.points <| points
        , Attributes.fill "none"
        ]
        []


renderCarCollisionDetection : Car -> Svg msg
renderCarCollisionDetection car =
    let
        points =
            Polygon2d.outerLoop car.shape |> toPointsString
    in
    Svg.polygon
        [ Attributes.points points
        , Attributes.fill <| Color.toCssString Color.red
        , Attributes.opacity "0.5"
        ]
        []


renderCarFieldOfView : Car -> Svg msg
renderCarFieldOfView car =
    let
        triangle =
            Car.fieldOfView car

        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle

        points =
            toPointsString [ p1, p2, p3 ]
    in
    Svg.polygon
        [ Attributes.points points
        , Attributes.fill <| Color.toCssString Color.purple
        , Attributes.opacity "0.3"
        ]
        []



--
-- Conversion
--


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


pointToPixels : LMPoint2d -> { x : Float, y : Float }
pointToPixels point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels


toPointsString : List LMPoint2d -> String
toPointsString points =
    List.foldl
        (\point acc ->
            let
                { x, y } =
                    pointToPixels point

                pointStr =
                    String.fromFloat x ++ "," ++ String.fromFloat (boardSizeScaledPixels - y) ++ " "
            in
            pointStr ++ acc
        )
        ""
        points



--
-- Assets
--


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
