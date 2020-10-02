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
        , uniform
        )
import Collage.Layout as Layout
import Collage.Render exposing (svg)
import Color
import Config exposing (boardSize)
import Dict
import Direction exposing (Direction(..), Orientation(..))
import Graphics
import Html exposing (Html)
import Lot exposing (Lot(..))
import Maybe.Extra as Maybe
import Position
import SharedState exposing (Lots, SharedState)
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
        [ renderCars (Dict.values cars) dimensions.tileSize lots
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
        emptyTile =
            square tileSize
                |> styled ( uniform renderColors.terrain, Collage.invisible )

        drawTile x y =
            board
                |> Board.get ( x, y )
                |> Maybe.map (renderTile tileSize)
                |> Maybe.withDefault emptyTile
    in
    Graphics.grid boardSize drawTile


renderTile : Float -> Tile -> Collage msg
renderTile tileSize tile =
    let
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


renderCars : List Car -> Float -> Lots -> Collage msg
renderCars cars tileSize lots =
    List.map (renderCar tileSize lots) cars
        |> Collage.group


renderCar : Float -> Lots -> Car -> Collage msg
renderCar tileSize lots car =
    let
        size =
            tileSize * 0.3

        currentLot =
            car.homeLotId
                |> Maybe.map (\id -> Dict.get id lots)
                |> Maybe.join

        ( shiftX, shiftY ) =
            case ( car.status, currentLot ) of
                ( ParkedAtLot, Just (Building kind _) ) ->
                    Lot.entryDirection kind
                        |> Position.shiftTo (floor (tileSize / 2)) ( 0, 0 )
                        |> Position.float

                _ ->
                    alignCarToLane size car

        position =
            car.position
                |> Position.float
                |> (\( x, y ) -> ( x * tileSize - tileSize + shiftX, shiftY + tileSize - y * tileSize ))

        rotationModifier =
            case car.status of
                Turning LeftTurn ->
                    -45

                Turning RightTurn ->
                    45

                _ ->
                    0

        rotation =
            dirToRotation car.direction
                + rotationModifier
                |> degrees
    in
    Graphics.texture size (Graphics.carAsset car)
        |> rotate rotation
        |> shift position


alignCarToLane : Float -> Car -> ( Float, Float )
alignCarToLane carSize car =
    let
        shiftAmount =
            carSize * 0.5

        baseShift =
            case car.status of
                Turning _ ->
                    shiftAmount

                _ ->
                    0
    in
    case car.direction of
        Up ->
            ( shiftAmount, baseShift )

        Right ->
            ( baseShift, -shiftAmount )

        Down ->
            ( -shiftAmount, -baseShift )

        Left ->
            ( -baseShift, shiftAmount )


dirToRotation : Direction -> Float
dirToRotation dir =
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
                    Position.next anchor dirFromRoad

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

                entryPointPosition =
                    Lot.entryDirection kind
                        |> Position.shiftTo sidewalkGapShift ( 0, 0 )

                -- sidewalk gap hides terrain between sizewalk and the lot
                -- Room for improvement: use special road tiles when connected to a lot
                sidewalkGap =
                    Collage.rectangle sidewalkGapWidth sidewalkGapHeight
                        |> styled ( uniform renderColors.sidewalk, invisible )
                        |> shift (Position.float entryPointPosition)
            in
            Collage.group
                [ sidewalkGap
                , Graphics.texture size (Graphics.buildingAsset kind)
                ]
                |> shift
                    (origin
                        |> Position.float
                        |> (\( x, y ) -> ( x * size - size, size - y * size ))
                    )
