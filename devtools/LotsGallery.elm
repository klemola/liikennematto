module LotsGallery exposing (..)

import Circle2d
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Colors as Colors
import Data.Lots
import Geometry.Svg as Svg
import Length
import Model.Cell
import Model.Geometry exposing (LMCubicSpline2d, LMPoint2d)
import Model.Lot exposing (Lot, ParkingSpot)
import Model.Tilemap as Tilemap
import Pixels
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Render
import Svg exposing (Svg)
import Svg.Attributes as Attributes


tilemapSizeScaledStr : String
tilemapSizeScaledStr =
    String.fromFloat Render.tilemapSizePixels


main : Svg msg
main =
    Data.Lots.allLots
        |> List.indexedMap
            (\id newLot ->
                let
                    id1Indexed =
                        id + 1

                    x =
                        if id == 1 then
                            3

                        else
                            id1Indexed + 2
                in
                Model.Cell.fromCoordinates ( x, 3 )
                    |> Maybe.map (Model.Lot.build id newLot)
            )
        |> List.filterMap identity
        |> List.map renderLotDebug
        |> Svg.svg
            [ Attributes.width tilemapSizeScaledStr
            , Attributes.height tilemapSizeScaledStr
            , Attributes.viewBox <| "0 0 " ++ tilemapSizeScaledStr ++ " " ++ tilemapSizeScaledStr
            , Attributes.style <| "background-color: " ++ Color.toCssString Colors.lightGreen ++ ";"
            ]


renderLotDebug : Lot -> Svg msg
renderLotDebug lot =
    Svg.g []
        [ Render.renderLot lot
        , Svg.g []
            (renderParkingSpotPaths lot.parkingSpots)
        ]


renderParkingSpotPaths : List ParkingSpot -> List (Svg msg)
renderParkingSpotPaths parkingSpots =
    List.indexedMap
        (\idx parkingSpot ->
            let
                opacity =
                    0.6 - (toFloat idx * 0.1)

                color =
                    case idx of
                        0 ->
                            Color.rgba 0 0 1 opacity

                        1 ->
                            Color.rgba 0 1 1 opacity

                        2 ->
                            Color.rgba 1 0 0 opacity

                        _ ->
                            Color.rgba 0 0 0 opacity
            in
            cubicSpline (flipSlineYCoordinate parkingSpot.pathToLotExit) color
        )
        parkingSpots


type SVGCoordinates
    = SVGCoordinates -- Y down instead of up


flipSlineYCoordinate : LMCubicSpline2d -> CubicSpline2d Length.Meters SVGCoordinates
flipSlineYCoordinate spline =
    let
        cp1 =
            CubicSpline2d.firstControlPoint spline |> flipPointYCoordinate

        cp2 =
            CubicSpline2d.secondControlPoint spline |> flipPointYCoordinate

        cp3 =
            CubicSpline2d.thirdControlPoint spline |> flipPointYCoordinate

        cp4 =
            CubicSpline2d.fourthControlPoint spline |> flipPointYCoordinate
    in
    CubicSpline2d.fromControlPoints cp1 cp2 cp3 cp4


flipPointYCoordinate : LMPoint2d -> Point2d Length.Meters SVGCoordinates
flipPointYCoordinate originalPoint =
    let
        newY =
            Tilemap.mapSize |> Quantity.minus (Point2d.yCoordinate originalPoint)
    in
    Point2d.xy
        (Point2d.xCoordinate originalPoint)
        newY


cubicSpline : CubicSpline2d Length.Meters SVGCoordinates -> Color -> Svg msg
cubicSpline spline color =
    let
        splinePixels =
            CubicSpline2d.at Render.pixelsToMetersRatio spline

        cssColor =
            Color.toCssString color

        controlPoints =
            [ CubicSpline2d.firstControlPoint splinePixels
            , CubicSpline2d.secondControlPoint splinePixels
            , CubicSpline2d.thirdControlPoint splinePixels
            , CubicSpline2d.fourthControlPoint splinePixels
            ]

        drawPoint point =
            Svg.circle2d [] <|
                Circle2d.withRadius (Pixels.pixels 2) point
    in
    Svg.g
        [ Attributes.stroke cssColor
        ]
        [ Svg.cubicSpline2d
            [ Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.fill "none"
            ]
            splinePixels
        , Svg.polyline2d
            [ Attributes.strokeWidth "2"
            , Attributes.fill "none"
            , Attributes.strokeDasharray "6 6"
            ]
            (Polyline2d.fromVertices controlPoints)
        , Svg.g [ Attributes.fill "rgba(255, 255, 255, 0.5)" ]
            (List.map drawPoint controlPoints)
        ]
