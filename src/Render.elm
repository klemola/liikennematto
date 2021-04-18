module Render exposing (view)

import Angle
import Board exposing (Board, Tile)
import Car exposing (Car, Cars, Status(..))
import Cell exposing (OrthogonalDirection(..))
import Circle2d
import Collage
    exposing
        ( Collage
        , invisible
        , solid
        , square
        , thin
        , traced
        , uniform
        )
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Color
import Config
    exposing
        ( boardSize
        , pixelsToMetersRatio
        , tileSize
        , tileSizeInMeters
        )
import Dict
import Direction2d
import Geometry exposing (LMPoint2d)
import Graph
import Graphics
import Html exposing (Html)
import Length exposing (Length)
import Lot exposing (Lot, Lots)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point2d
import Quantity exposing (Quantity)
import RoadNetwork exposing (ConnectionKind(..), RoadNetwork)
import TrafficLight exposing (TrafficLight, TrafficLightColor(..), TrafficLights)
import Triangle2d
import Vector2d
import World exposing (World)


type alias DebugLayers =
    -- Room for improvement: separate debug views from regular views
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }


nodeSize : Quantity Float Pixels
nodeSize =
    Pixels.float 4


trafficLightRadius : Quantity Float Pixels
trafficLightRadius =
    Pixels.float 5


carWidthPixels =
    toPixelsValue Car.width


carLengthPixels =
    toPixelsValue Car.length


view : World -> DebugLayers -> Html msg
view { board, cars, lots, roadNetwork, trafficLights } { showRoadNetwork, showCarDebugVisuals } =
    let
        base =
            renderBoard board
                |> Layout.at Layout.bottomLeft
                    (renderLots lots)
                |> Layout.at Layout.bottomLeft (renderCars cars showCarDebugVisuals)
                |> Layout.at Layout.bottomLeft (renderTrafficLights trafficLights)

        withConditionalLayers =
            if showRoadNetwork then
                base
                    |> Layout.at Layout.bottomLeft (renderRoadNetwork roadNetwork)

            else
                base
    in
    withConditionalLayers
        |> svg


renderColors =
    { road = Color.rgb255 52 65 67
    , terrain = Color.rgb255 33 191 154
    , sidewalk = Color.rgb255 191 213 217
    , sidewalkEdge = Color.rgb255 44 56 58
    }


renderBoard : Board -> Collage msg
renderBoard board =
    let
        emptyTile =
            square (Pixels.inPixels tileSize)
                |> Collage.styled ( uniform renderColors.terrain, Collage.invisible )

        drawTile x y =
            board
                |> Dict.get ( x, y )
                |> Maybe.map renderTile
                |> Maybe.withDefault emptyTile
    in
    Graphics.grid boardSize drawTile


renderTile : Tile -> Collage msg
renderTile tile =
    let
        renderedSize =
            Graphics.renderedSizeFromUnits ( 1, 1 ) (Pixels.inPixels tileSize)
    in
    tile
        |> Graphics.tileAsset
        |> Graphics.texture renderedSize


renderCars : Cars -> Bool -> Collage msg
renderCars cars showCarDebugVisuals =
    cars
        |> Dict.foldl
            (\_ car acc ->
                let
                    renderedCar =
                        if showCarDebugVisuals then
                            renderCar car
                                |> renderWithDebug car renderCarDebug

                        else
                            renderCar car
                in
                renderedCar :: acc
            )
            []
        |> Collage.group


renderCar : Car -> Collage msg
renderCar car =
    Graphics.texture ( carLengthPixels, carWidthPixels ) (Graphics.carAsset car)
        |> Collage.rotate (Angle.inRadians car.rotation)
        |> Collage.shift (toPixelsTuple car.position)


renderLots : Lots -> Collage msg
renderLots lots =
    lots
        |> Dict.foldl (\_ lot acc -> renderLot lot :: acc) []
        |> Collage.group


renderLot : Lot -> Collage msg
renderLot lot =
    let
        displacement =
            Vector2d.xy
                (lot.width |> Quantity.divideBy 2)
                (lot.height |> Quantity.divideBy 2)

        lotCenterPoint =
            lot.position
                |> Point2d.translateBy displacement
                |> toPixelsTuple

        building =
            Graphics.texture
                ( toPixelsValue lot.width, toPixelsValue lot.height )
                (Graphics.buildingAsset lot.content.kind)

        mask =
            sidewalkMask lot
    in
    building
        |> Layout.at Layout.bottomLeft mask
        |> Collage.shift lotCenterPoint


sidewalkMask : Lot -> Collage msg
sidewalkMask lot =
    -- sidewalk mask hides terrain between sidewalk and the lot
    -- Room for improvement: use special road tiles when connected to a lot
    Collage.rectangle
        (toPixelsValue lot.entryDetails.width)
        (toPixelsValue lot.entryDetails.height)
        |> Collage.styled ( uniform renderColors.sidewalk, invisible )
        |> Collage.shift (toPixelsTuple lot.entryDetails.entryPoint)


renderRoadNetwork : RoadNetwork -> Collage msg
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
                |> Graph.nodes
                |> List.map
                    (\node ->
                        Collage.circle (Pixels.inPixels nodeSize)
                            |> Collage.styled ( uniform (nodeColor node.label.kind), invisible )
                            |> Collage.shift (toPixelsTuple node.label.position)
                    )
                |> Collage.group

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
                                        toPixelsTuple fromNodeCtx.node.label.position

                                    to =
                                        toPixelsTuple toNodeCtx.node.label.position
                                in
                                Collage.segment from to
                                    |> traced (solid thin (uniform Color.orange))
                            )
                            fromNode
                            toNode
                    )
                |> Collage.group
    in
    Collage.group
        [ nodes
        , edges
        ]


renderTrafficLights : TrafficLights -> Collage msg
renderTrafficLights trafficLights =
    trafficLights
        |> Dict.foldl (\_ tl acc -> renderTrafficLight tl :: acc) []
        |> Collage.group


renderTrafficLight : TrafficLight -> Collage msg
renderTrafficLight trafficLight =
    let
        borderSize =
            1

        border =
            solid borderSize <| uniform Color.grey

        color =
            case trafficLight.color of
                Green ->
                    Color.darkGreen

                Yellow ->
                    Color.darkYellow

                Red ->
                    Color.darkRed
    in
    Collage.circle (Pixels.inPixels trafficLightRadius)
        |> Collage.styled ( uniform color, border )
        |> Collage.shift (toPixelsTuple trafficLight.position)



--
-- Debug
--


renderWithDebug : a -> (a -> Collage msg) -> Collage msg -> Collage msg
renderWithDebug debuggedThing debugFn collage =
    Collage.group
        [ collage
        , debugFn debuggedThing
        ]


renderCarDebug : Car -> Collage msg
renderCarDebug car =
    Collage.group
        [ renderCarFieldOfView car
        , renderCarCollisionDetection car
        , renderCarPath car
        ]


renderCarPath : Car -> Collage msg
renderCarPath car =
    car.localPath
        |> List.map toPixelsTuple
        |> Collage.path
        |> traced (solid thin (uniform Color.red))


renderCarCollisionDetection : Car -> Collage msg
renderCarCollisionDetection car =
    -- Room for improvement: move collision check area logic to a shared module, so that duplicate code here can be removed
    let
        carDirection =
            Direction2d.fromAngle car.rotation

        forwardShiftedCarPosition =
            car.position
                |> Point2d.translateIn carDirection (Car.length |> Quantity.divideBy 2)
    in
    Collage.circle
        (Circle2d.atPoint forwardShiftedCarPosition (Car.length |> Quantity.divideBy 2)
            |> Circle2d.radius
            |> toPixelsValue
        )
        |> Collage.styled ( uniform Color.blue, invisible )
        |> Collage.shift (toPixelsTuple forwardShiftedCarPosition)
        |> Collage.opacity 0.5


renderCarFieldOfView : Car -> Collage msg
renderCarFieldOfView car =
    let
        triangle =
            Car.rightSideOfFieldOfView tileSizeInMeters car

        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Collage.polygon
        [ toPixelsTuple p1
        , toPixelsTuple p2
        , toPixelsTuple p3
        ]
        |> Collage.styled ( uniform Color.purple, invisible )
        |> Collage.opacity 0.3



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
