module Render exposing (view)

import Angle
import Board exposing (Board, Tile)
import Car exposing (Car, Status(..))
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
        , carFieldOfView
        , carLength
        , carWidth
        , tileSize
        )
import Dict
import Direction2d
import Geometry exposing (toLMUnits)
import Graph
import Graphics
import Html exposing (Html)
import Lot exposing (Lot)
import Maybe.Extra as Maybe
import RoadNetwork exposing (ConnectionKind(..), RoadNetwork)
import TrafficLight exposing (TrafficLight, TrafficLightColor(..))
import Triangle2d
import World exposing (World)


type alias DebugLayers =
    -- Room for improvement: separate debug views from regular views
    { showRoadNetwork : Bool
    , showCarDebugVisuals : Bool
    }


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
            square tileSize
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
            Graphics.renderedSizeFromUnits ( 1, 1 ) tileSize
    in
    tile
        |> Graphics.tileAsset
        |> Graphics.texture renderedSize


renderCars : World.Cars -> Bool -> Collage msg
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
    Graphics.texture ( carLength, carWidth ) (Graphics.carAsset car)
        |> Collage.rotate (Angle.inRadians car.rotation)
        |> Collage.shift (Geometry.pointToPositionAsTuple car.position)


renderLots : World.Lots -> Collage msg
renderLots lots =
    lots
        |> Dict.foldl (\_ lot acc -> renderLot lot :: acc) []
        |> Collage.group


renderLot : Lot -> Collage msg
renderLot lot =
    let
        lotCenterPoint =
            lot.position
                |> Geometry.translatePointBy (lot.width / 2) (lot.height / 2)
                |> Geometry.pointToPositionAsTuple

        building =
            Graphics.texture ( lot.width, lot.height ) (Graphics.buildingAsset lot.content.kind)

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
    let
        maskSize =
            tileSize / 6

        maskOverlap =
            tileSize / 16

        ( maskWidth, maskHeight ) =
            if Cell.isVertical <| Tuple.second lot.anchor then
                ( tileSize / 2, maskSize )

            else
                ( maskSize, tileSize / 2 )

        entryPointPosition =
            case lot.content.entryDirection of
                Up ->
                    ( lot.width - tileSize / 2, lot.height + maskOverlap )

                Right ->
                    ( lot.width + maskOverlap, lot.height - tileSize / 2 )

                Down ->
                    ( tileSize / 2, -maskOverlap )

                Left ->
                    ( -maskOverlap, tileSize / 2 )
    in
    Collage.rectangle maskWidth maskHeight
        |> Collage.styled ( uniform renderColors.sidewalk, invisible )
        |> Collage.shift entryPointPosition


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
                        Collage.circle Config.nodeSize
                            |> Collage.styled ( uniform (nodeColor node.label.kind), invisible )
                            |> Collage.shift (Geometry.pointToPositionAsTuple node.label.position)
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
                                        Geometry.pointToPositionAsTuple fromNodeCtx.node.label.position

                                    to =
                                        Geometry.pointToPositionAsTuple toNodeCtx.node.label.position
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


renderTrafficLights : World.TrafficLights -> Collage msg
renderTrafficLights trafficLights =
    trafficLights
        |> Dict.foldl (\_ tl acc -> renderTrafficLight tl :: acc) []
        |> Collage.group


renderTrafficLight : TrafficLight -> Collage msg
renderTrafficLight trafficLight =
    let
        markerSize =
            tileSize * 0.1

        borderSize =
            markerSize * 0.16

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
    Collage.circle markerSize
        |> Collage.styled ( uniform color, border )
        |> Collage.shift (Geometry.pointToPositionAsTuple trafficLight.position)



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
        |> List.map Geometry.pointToPositionAsTuple
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
                |> Geometry.translatePointIn carDirection (toLMUnits <| carLength / 2)
    in
    Collage.circle
        (Geometry.circleAt forwardShiftedCarPosition (carLength / 2)
            |> Circle2d.radius
            |> Geometry.toFloat
        )
        |> Collage.styled ( uniform Color.blue, invisible )
        |> Collage.shift (Geometry.pointToPositionAsTuple forwardShiftedCarPosition)
        |> Collage.opacity 0.5


renderCarFieldOfView : Car -> Collage msg
renderCarFieldOfView car =
    let
        carDirection =
            Direction2d.fromAngle car.rotation

        triangle =
            Geometry.fieldOfViewTriangle car.position carDirection carFieldOfView (Geometry.toLMUnits tileSize)

        ( p1, p2, p3 ) =
            Triangle2d.vertices triangle
    in
    Collage.polygon
        [ Geometry.pointToPositionAsTuple p1
        , Geometry.pointToPositionAsTuple p2
        , Geometry.pointToPositionAsTuple p3
        ]
        |> Collage.styled ( uniform Color.purple, invisible )
        |> Collage.opacity 0.3
