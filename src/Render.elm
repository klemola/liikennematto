module Render exposing (view)

import Board exposing (Board)
import Car exposing (Car, Status(..), TurnKind(..))
import Collage
    exposing
        ( Collage
        , circle
        , invisible
        , rotate
        , shift
        , solid
        , square
        , styled
        , thin
        , traced
        , uniform
        )
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Color
import Config exposing (boardSize, carSize, tileSize)
import Dict
import Direction exposing (Direction(..), Orientation(..))
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
import TrafficLight exposing (TrafficLight, TrafficLightKind(..))
import World exposing (Lots, World)


view : World -> Html msg
view { board, cars, lots, roadNetwork } =
    renderBoard board
        |> Layout.at Layout.bottomLeft
            (renderLots (Dict.values lots))
        |> Layout.at Layout.bottomLeft (renderCars (Dict.values cars) lots)
        |> Layout.at Layout.bottomLeft (renderRoadNetwork roadNetwork)
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
                |> styled ( uniform renderColors.terrain, Collage.invisible )

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

        intersection shape content =
            Layout.stack (content ++ [ Graphics.texture renderedSize (Graphics.intersectionAsset shape) ])

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

        Intersection (Signal trafficLights) shape ->
            trafficLights
                |> List.map renderTrafficLight
                |> intersection shape

        Intersection (Yield orientation) shape ->
            renderSigns orientation shape "yield_sign.png"
                |> intersection shape

        Intersection (Stop orientation) shape ->
            renderSigns orientation shape "stop_sign.png"
                |> intersection shape

        Intersection Uncontrolled shape ->
            intersection shape []


renderTrafficLight : TrafficLight -> Collage msg
renderTrafficLight tl =
    let
        markerSize =
            tileSize * 0.1

        borderSize =
            markerSize * 0.16

        offset =
            markerSize + (2 * borderSize)

        border =
            solid borderSize <| uniform Color.grey

        presentation =
            circle markerSize
                |> styled ( uniform (toColor tl.kind), border )

        toColor tlKind =
            case tlKind of
                Green ->
                    Color.darkGreen

                Yellow ->
                    Color.darkYellow

                Red ->
                    Color.darkRed
    in
    Graphics.marker offset tl.facing presentation


renderSigns : Orientation -> IntersectionShape -> String -> List (Collage msg)
renderSigns orientation intersectionShape asset =
    let
        size =
            tileSize * 0.25

        offset =
            size * 0.5

        locations =
            case intersectionShape of
                T dir ->
                    [ dir ]

                Crossroads ->
                    Direction.fromOrientation orientation

        presentation dir =
            Graphics.texture ( size, size ) asset
                |> Graphics.marker offset dir
    in
    locations
        |> List.map presentation


renderCars : List Car -> Lots -> Collage msg
renderCars cars lots =
    List.map (renderCar lots) cars
        |> Collage.group


renderCar : Lots -> Car -> Collage msg
renderCar lots car =
    let
        currentLot =
            car.homeLotId
                |> Maybe.map (\id -> Dict.get id lots)
                |> Maybe.join
    in
    Graphics.texture ( carSize, carSize ) (Graphics.carAsset car)
        |> rotate car.rotation
        |> shift car.position


renderLots : List Lot -> Collage msg
renderLots lots =
    List.map renderLot lots
        |> Collage.group


renderLot : Lot -> Collage msg
renderLot lot =
    let
        ( x, y ) =
            lot.position

        building =
            Graphics.texture ( lot.width, lot.height ) (Graphics.buildingAsset lot.content.kind)

        mask =
            sidewalkMask lot
    in
    building
        |> Layout.at Layout.bottomLeft mask
        |> shift ( x + lot.width / 2, y + lot.height / 2 )


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
            case Direction.toOrientation (Tuple.second lot.anchor) of
                Vertical ->
                    ( tileSize / 2, maskSize )

                Horizontal ->
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
        |> styled ( uniform renderColors.sidewalk, invisible )
        |> shift entryPointPosition


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
                            |> styled ( uniform (nodeColor node.label.kind), invisible )
                            |> shift node.label.position
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
                                Collage.segment fromNodeCtx.node.label.position toNodeCtx.node.label.position
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
