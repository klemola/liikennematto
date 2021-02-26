module Render exposing (view)

import Angle
import Board exposing (Board)
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
import Config exposing (boardSize, carFieldOfView, carLength, carWidth, tileSize)
import Dict
import Direction2d
import Geometry
import Graph
import Graphics
import Html exposing (Html)
import Lot exposing (Lot)
import Maybe.Extra as Maybe
import RoadNetwork exposing (ConnectionKind(..), RoadNetwork)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import Triangle2d
import World exposing (World)


view : World -> Bool -> Html msg
view { board, cars, lots, roadNetwork } showRoadNetwork =
    let
        base =
            renderBoard board
                |> Layout.at Layout.bottomLeft
                    (renderLots (Dict.values lots))
                |> Layout.at Layout.bottomLeft (renderCars (Dict.values cars))

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

        addRoadMarkings roadKind trafficDirection road =
            case trafficDirection of
                Both ->
                    road

                OneWay ->
                    Layout.stack [ Graphics.texture renderedSize (Graphics.oneWayMarker roadKind), road ]
    in
    case tile of
        TwoLaneRoad kind trafficDirection ->
            Graphics.roadAsset kind
                |> Graphics.texture renderedSize
                |> addRoadMarkings kind trafficDirection

        Intersection _ shape ->
            Graphics.texture renderedSize (Graphics.intersectionAsset shape)


renderCars : List Car -> Collage msg
renderCars cars =
    List.map renderCar cars
        |> Collage.group


renderCar : Car -> Collage msg
renderCar car =
    Collage.group
        [ Graphics.texture ( carLength, carWidth ) (Graphics.carAsset car)
            |> Collage.rotate (Angle.inRadians car.rotation)
            |> Collage.shift (Geometry.pointToPositionAsTuple car.position)
        , renderCarFieldOfView car
        , renderCarCollisionDetection car
        , renderCarPath car
        ]


renderLots : List Lot -> Collage msg
renderLots lots =
    List.map renderLot lots
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



-- Debug


renderCarPath : Car -> Collage msg
renderCarPath car =
    car.localPath
        |> List.map Geometry.pointToPositionAsTuple
        |> Collage.path
        |> traced (solid thin (uniform Color.red))


renderCarCollisionDetection : Car -> Collage msg
renderCarCollisionDetection car =
    Collage.circle
        (Geometry.circleAt car.position (carLength / 2)
            |> Circle2d.radius
            |> Geometry.toFloat
        )
        |> Collage.styled ( uniform Color.blue, invisible )
        |> Collage.shift (Geometry.pointToPositionAsTuple car.position)
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
        |> Collage.opacity 0.5
