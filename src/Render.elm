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
        , transparent
        , uniform
        )
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Color
import Config exposing (boardSize)
import Coords
import Dict
import Direction exposing (Direction(..), Orientation(..))
import Graphics
import Html exposing (Html)
import Lot exposing (Lot(..))
import SharedState exposing (SharedState)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight exposing (TrafficLight, TrafficLightKind(..))


view : SharedState -> Html msg
view { board, cars, lots, dimensions } =
    Layout.stack
        [ renderCars (Dict.values cars) dimensions.tileSize
        , renderLots (Dict.values lots) dimensions.tileSize
        , renderBoard board dimensions.tileSize
        ]
        |> svg


renderColors =
    { road = Color.rgb255 52 65 67
    , terrain = Color.rgb255 33 191 154
    , sidewalk = Color.rgb255 191 213 217
    , sidewalkEdge = Color.rgb255 44 56 58
    }


renderBoard : Board -> Float -> Collage msg
renderBoard board tileSize =
    let
        drawTile x y =
            board
                |> Board.getSafe ( x, y )
                |> renderTile tileSize
    in
    Graphics.grid boardSize drawTile


renderTile : Float -> Tile -> Collage msg
renderTile tileSize tile =
    let
        ground color =
            square tileSize
                |> styled ( uniform color, Collage.invisible )

        intersection shape content =
            Layout.stack (content ++ [ Graphics.texture tileSize (Graphics.intersectionAsset shape) ])

        addRoadMarkings roadKind trafficDirection road =
            case trafficDirection of
                Both ->
                    road

                OneWay ->
                    Layout.stack [ Graphics.texture tileSize (Graphics.oneWayMarker roadKind), road ]
    in
    case tile of
        TwoLaneRoad kind trafficDirection ->
            Graphics.roadAsset kind
                |> Graphics.texture tileSize
                |> addRoadMarkings kind trafficDirection

        Intersection (Signal trafficLights) shape ->
            trafficLights
                |> List.map (renderTrafficLight tileSize)
                |> intersection shape

        Intersection (Yield orientation) shape ->
            renderSigns tileSize orientation shape "yield_sign.png"
                |> intersection shape

        Intersection (Stop orientation) shape ->
            renderSigns tileSize orientation shape "stop_sign.png"
                |> intersection shape

        Intersection Uncontrolled shape ->
            intersection shape []

        Terrain ->
            ground renderColors.terrain


renderTrafficLight : Float -> TrafficLight -> Collage msg
renderTrafficLight tileSize tl =
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
    Graphics.marker tileSize offset tl.facing presentation


renderSigns : Float -> Orientation -> IntersectionShape -> String -> List (Collage msg)
renderSigns tileSize orientation intersectionShape asset =
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
            Graphics.texture size asset
                |> Graphics.marker tileSize offset dir
    in
    locations
        |> List.map presentation


renderCars : List Car -> Float -> Collage msg
renderCars cars tileSize =
    let
        carSize =
            tileSize * 0.3

        shiftAmount =
            carSize * 0.5

        -- fake tiles align the cars to the board beneath
        fakeTile =
            square tileSize
                |> styled ( transparent, invisible )

        baseShift status =
            case status of
                Turning _ ->
                    shiftAmount

                _ ->
                    0

        carShiftCoords status dir =
            case dir of
                Up ->
                    ( shiftAmount, baseShift status )

                Right ->
                    ( baseShift status, -shiftAmount )

                Down ->
                    ( -shiftAmount, -(baseShift status) )

                Left ->
                    ( -(baseShift status), shiftAmount )

        drawCars x y =
            Coords.filterBy cars ( x, y )
                |> List.map
                    (\c ->
                        renderCar carSize c
                            |> shift (carShiftCoords c.status c.direction)
                    )
                |> List.append [ fakeTile ]
                |> Layout.stack
    in
    -- cars are rendered as an overlaid grid of the same size as the board
    Graphics.grid boardSize drawCars


renderCar : Float -> Car -> Collage msg
renderCar size car =
    let
        rotationModifier =
            case car.status of
                Turning LeftTurn ->
                    -45

                Turning RightTurn ->
                    45

                _ ->
                    0

        rotation =
            rotationDegrees car.direction + rotationModifier
    in
    Graphics.texture size (Graphics.carAsset car)
        |> rotate (degrees rotation)


renderLots : List Lot -> Float -> Collage msg
renderLots lots tileSize =
    List.map (renderLot tileSize) lots
        |> Collage.group


renderLot : Float -> Lot -> Collage msg
renderLot size lot =
    case lot of
        Building kind ( anchor, dirFromRoad ) ->
            let
                origin =
                    Coords.next anchor dirFromRoad

                sidewalkGapSize =
                    size / 6

                sidewalkGapShift =
                    floor (size / 2 + (sidewalkGapSize / 2))

                ( sidewalkGapWidth, sidewalkGapHeight ) =
                    case Direction.toOrientation dirFromRoad of
                        Vertical ->
                            ( size / 2, sidewalkGapSize )

                        Horizontal ->
                            ( sidewalkGapSize, size / 2 )

                entryPointCoords =
                    Lot.entryDirection kind
                        |> Coords.shiftTo sidewalkGapShift ( 0, 0 )

                -- sidewalk gap hides terrain between sizewalk and the lot
                -- Room for improvement: use special road tiles when connected to a lot
                sidewalkGap =
                    Collage.rectangle sidewalkGapWidth sidewalkGapHeight
                        |> styled ( uniform renderColors.sidewalk, invisible )
                        |> shift (Coords.float entryPointCoords)
            in
            Collage.group
                [ sidewalkGap
                , Graphics.texture size (Graphics.buildingAsset kind)
                ]
                |> shift
                    (origin
                        |> Coords.float
                        |> (\( x, y ) -> ( x * size - size, size - y * size ))
                    )


rotationDegrees : Direction -> Float
rotationDegrees dir =
    -- the values here are based on Collage logic: counter-clockwise rotation (from "Up")
    case dir of
        Up ->
            0

        Right ->
            270

        Down ->
            180

        Left ->
            90
