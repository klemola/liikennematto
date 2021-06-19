module Render exposing (view)

import Angle
import Board exposing (Board, Tile)
import Car exposing (Car, Cars, Status(..))
import Cell exposing (Cell, OrthogonalDirection(..))
import Color
import Config
    exposing
        ( boardSizeScaled
        , pixelsToMetersRatio
        , tileSize
        )
import Dict
import Geometry exposing (LMPoint2d)
import Graph
import Graphics
import Html exposing (Html)
import Length exposing (Length)
import Lot exposing (Lot, Lots)
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
    -- Room for improvement: separate debug views from regular views
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


carWidthPixels =
    toPixelsValue Car.width


carLengthPixels =
    toPixelsValue Car.length


boardSizeScaledPixels =
    boardSizeScaled |> Pixels.inPixels |> toFloat


tileSizePixels =
    tileSize |> Pixels.inPixels


view : World -> DebugLayers -> Html msg
view { board, cars, lots, roadNetwork, trafficLights } debugLayers =
    Svg.svg [ Attributes.width "800", Attributes.height "800" ]
        ([ Svg.Lazy.lazy renderBoard board
         , Svg.Lazy.lazy renderLots lots
         , renderCars cars
         , Svg.Lazy.lazy renderTrafficLights trafficLights
         , renderTrafficSigns roadNetwork
         ]
            ++ renderDebugLayers debugLayers cars roadNetwork
        )


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
            "assets/" ++ Graphics.tileAsset tile

        ( x, y ) =
            Cell.bottomLeftCorner cell |> toPixelsTuple
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
            "assets/" ++ Graphics.carAsset car

        ( x, y ) =
            toPixelsTuple car.position

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
        |> Dict.foldl (\_ lot acc -> ( Graphics.buildingAsset lot.content.kind, renderLot lot ) :: acc) []
        |> Svg.Keyed.node "g" []


renderLot : Lot -> Svg msg
renderLot lot =
    let
        ( x, y ) =
            toPixelsTuple lot.position

        asset =
            "assets/" ++ Graphics.buildingAsset lot.content.kind

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
        ( lotX, lotY ) =
            toPixelsTuple lot.position

        ( x, y ) =
            toPixelsTuple lot.entryDetails.entryPoint

        width =
            toPixelsValue lot.entryDetails.width

        height =
            toPixelsValue lot.entryDetails.height
    in
    Svg.rect
        [ Attributes.width <| String.fromFloat width
        , Attributes.height <| String.fromFloat height
        , Attributes.fill <| Color.toCssString <| renderColors.sidewalk
        , Attributes.x <| String.fromFloat <| x + lotX - (width / 2)
        , Attributes.y <| String.fromFloat <| boardSizeScaledPixels - lotY - y - (height / 2)
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
        ( x, y ) =
            toPixelsTuple trafficLight.position

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
    let
        size =
            Pixels.inPixels signSize
    in
    roadNetwork
        |> Graph.nodes
        |> List.filterMap
            (\node ->
                case node.label.trafficControl of
                    Yield ->
                        let
                            ( x, y ) =
                                toPixelsTuple node.label.position

                            asset =
                                "assets/yield_sign.png"
                        in
                        Just
                            ( "Yield-" ++ String.fromInt node.id
                            , Svg.image
                                [ Attributes.xlinkHref asset
                                , Attributes.x <| String.fromFloat (x - size / 2)
                                , Attributes.y <| String.fromFloat <| boardSizeScaledPixels - y - (size / 2)
                                , Attributes.width <| String.fromFloat size
                                , Attributes.height <| String.fromFloat size
                                ]
                                []
                            )

                    _ ->
                        Nothing
            )
        |> Svg.Keyed.node "g" []



--
-- Debug
--


renderDebugLayers : DebugLayers -> Cars -> RoadNetwork -> List (Svg msg)
renderDebugLayers { showRoadNetwork, showCarDebugVisuals } cars roadNetwork =
    -- let
    --     roadNetworkDebug =
    --         if showRoadNetwork then
    --             renderRoadNetwork roadNetwork |> Collage.Render.svgBox ( boardSizeScaledPixels, boardSizeScaledPixels )
    --         else
    --             Html.div [] []
    -- in
    []



-- renderRoadNetwork : RoadNetwork -> Collage msg
-- renderRoadNetwork roadNetwork =
--     let
--         nodeColor kind =
--             case kind of
--                 LaneStart ->
--                     Color.white
--                 LaneEnd ->
--                     Color.lightBlue
--                 DeadendEntry ->
--                     Color.lightPurple
--                 DeadendExit ->
--                     Color.darkGray
--                 LotEntry ->
--                     Color.lightGreen
--                 Stopgap ->
--                     Color.lightYellow
--         nodes =
--             roadNetwork
--                 |> Graph.nodes
--                 |> List.map
--                     (\node ->
--                         Collage.circle (Pixels.inPixels nodeSize)
--                             |> Collage.styled ( uniform (nodeColor node.label.kind), invisible )
--                             |> Collage.shift (toPixelsTuple node.label.position)
--                     )
--                 |> Collage.group
--         edges =
--             roadNetwork
--                 |> Graph.edges
--                 |> List.filterMap
--                     (\edge ->
--                         let
--                             fromNode =
--                                 Graph.get edge.from roadNetwork
--                             toNode =
--                                 Graph.get edge.to roadNetwork
--                         in
--                         Maybe.map2
--                             (\fromNodeCtx toNodeCtx ->
--                                 let
--                                     from =
--                                         toPixelsTuple fromNodeCtx.node.label.position
--                                     to =
--                                         toPixelsTuple toNodeCtx.node.label.position
--                                 in
--                                 Collage.segment from to
--                                     |> traced (solid thin (uniform Color.orange))
--                             )
--                             fromNode
--                             toNode
--                     )
--                 |> Collage.group
--     in
--     Collage.group
--         [ nodes
--         , edges
--         ]
-- renderCarDebug : Car -> Svg msg
-- renderCarDebug car =
--     Svg.g []
--         [ renderCarFieldOfView car
--         , renderCarCollisionDetection car
--         , renderCarPath car
--         ]
-- renderCarPath : Car -> Collage msg
-- renderCarPath car =
--     car.localPath
--         |> List.map toPixelsTuple
--         |> Collage.path
--         |> traced (solid thin (uniform Color.red))
-- renderCarCollisionDetection : Car -> Collage msg
-- renderCarCollisionDetection car =
--     let
--         carShape =
--             Polygon2d.outerLoop car.shape |> List.map toPixelsTuple
--     in
--     carShape
--         |> Collage.polygon
--         |> Collage.styled ( uniform Color.red, invisible )
--         |> Collage.opacity 0.5
-- renderCarFieldOfView : Car -> Collage msg
-- renderCarFieldOfView car =
--     let
--         triangle =
--             Car.fieldOfView car
--         ( p1, p2, p3 ) =
--             Triangle2d.vertices triangle
--     in
--     Collage.polygon
--         [ toPixelsTuple p1
--         , toPixelsTuple p2
--         , toPixelsTuple p3
--         ]
--         |> Collage.styled ( uniform Color.purple, invisible )
--         |> Collage.opacity 0.3
--
-- Conversion
--


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


toPixelsTuple : LMPoint2d -> ( Float, Float )
toPixelsTuple point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toTuple Pixels.inPixels
