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
import Config exposing (boardSize, tileSize)
import Dict
import Direction exposing (Direction(..), Orientation(..))
import Graphics
import Html exposing (Html)
import Lot exposing (Lot(..))
import Maybe.Extra as Maybe
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
view { board, cars, lots } =
    renderBoard board
        |> Layout.at Layout.bottomLeft
            (renderLots (Dict.values lots))
        |> Layout.at Layout.bottomLeft (renderCars (Dict.values cars) lots)
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
                |> Board.get ( x, y )
                |> Maybe.map (renderTile tileSize)
                |> Maybe.withDefault emptyTile
    in
    Graphics.grid boardSize drawTile


renderTile : Float -> Tile -> Collage msg
renderTile tileSize tile =
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
    Graphics.marker offset tl.facing presentation


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
        size =
            tileSize * 0.3

        currentLot =
            car.homeLotId
                |> Maybe.map (\id -> Dict.get id lots)
                |> Maybe.join

        ( shiftX, shiftY ) =
            case ( car.status, currentLot ) of
                ( ParkedAtLot, Just (Building props _ _) ) ->
                    alignCarToDriveway props

                _ ->
                    alignCarToLane size car

        ( x, y ) =
            car.position

        position =
            ( x + shiftX, y + shiftY )

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
    Graphics.texture ( size, size ) (Graphics.carAsset car)
        |> rotate rotation
        |> shift position


alignCarToDriveway : Lot.BuildingProperties -> ( Float, Float )
alignCarToDriveway { entryDirection } =
    case entryDirection of
        Up ->
            ( tileSize / 2, tileSize )

        Right ->
            ( tileSize, tileSize / 2 )

        Down ->
            ( tileSize / 2, 0 )

        Left ->
            ( 0, tileSize / 2 )


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


renderLots : List Lot -> Collage msg
renderLots lots =
    List.map renderLot lots
        |> Collage.group


renderLot : Lot -> Collage msg
renderLot lot =
    case lot of
        Building buildingProps ( x, y ) anchor ->
            let
                { width, height } =
                    buildingProps

                building =
                    Graphics.texture ( width, height ) (Graphics.buildingAsset buildingProps.kind)

                mask =
                    sidewalkMask anchor buildingProps
            in
            building
                |> Layout.at Layout.bottomLeft mask
                |> shift ( x + buildingProps.width / 2, y + buildingProps.height / 2 )


sidewalkMask : Lot.Anchor -> Lot.BuildingProperties -> Collage msg
sidewalkMask ( _, dirFromRoad ) buildingProps =
    -- sidewalk mask hides terrain between sidewalk and the lot
    -- Room for improvement: use special road tiles when connected to a lot
    let
        maskSize =
            tileSize / 6

        maskOverlap =
            tileSize / 16

        ( maskWidth, maskHeight ) =
            case Direction.toOrientation dirFromRoad of
                Vertical ->
                    ( tileSize / 2, maskSize )

                Horizontal ->
                    ( maskSize, tileSize / 2 )

        entryPointPosition =
            -- Room for improvement: should not assume that bigger lots have their entry points in a corner
            case buildingProps.entryDirection of
                Up ->
                    ( tileSize / 2, tileSize + maskOverlap )

                Right ->
                    ( tileSize + maskOverlap, tileSize / 2 )

                Down ->
                    ( tileSize / 2, -maskOverlap )

                Left ->
                    ( -maskOverlap, tileSize / 2 )
    in
    Collage.rectangle maskWidth maskHeight
        |> styled ( uniform renderColors.sidewalk, invisible )
        |> shift entryPointPosition
