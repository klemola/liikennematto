module LotsGallery exposing (..)

import Circle2d
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Colors as Colors
import Data.Lots
import Geometry.Svg as Svg
import Length exposing (Length)
import Model.Cell
import Model.Geometry exposing (LMCubicSpline2d, LMPoint2d)
import Model.Lot exposing (Lot, ParkingSpot)
import Model.RenderCache as RenderCache
import Model.Tilemap as Tilemap
import Model.World as World
import Pixels
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Render
import Render.Conversion
import Svg exposing (Svg)
import Svg.Attributes as Attributes


main : Svg msg
main =
    let
        tilemapConfig =
            { horizontalCellsAmount = 20
            , verticalCellsAmount = 4
            }

        world =
            World.empty tilemapConfig

        renderCache =
            RenderCache.new world

        tilemapHeight =
            Tilemap.dimensions world.tilemap |> .height

        tilemapWidthStr =
            String.fromFloat renderCache.tilemapWidthPixels

        tilemapHeightStr =
            String.fromFloat renderCache.tilemapHeightPixels
    in
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
                Model.Cell.fromCoordinates tilemapConfig ( x, 3 )
                    |> Maybe.map (Model.Lot.build tilemapConfig id newLot)
            )
        |> List.filterMap identity
        |> List.map (renderLotDebug tilemapHeight renderCache)
        |> Svg.svg
            [ Attributes.width tilemapWidthStr
            , Attributes.height tilemapHeightStr
            , Attributes.viewBox <| "0 0 " ++ tilemapWidthStr ++ " " ++ tilemapHeightStr
            , Attributes.style <| "background-color: " ++ Color.toCssString Colors.lightGreen ++ ";"
            ]


renderLotDebug : Length -> RenderCache.RenderCache -> Lot -> Svg msg
renderLotDebug tilemapHeight renderCache lot =
    Svg.g []
        [ Render.renderLot renderCache.tilemapHeightPixels lot
        , Svg.g []
            (renderParkingSpotPaths tilemapHeight lot.parkingSpots)
        ]


renderParkingSpotPaths : Length -> List ParkingSpot -> List (Svg msg)
renderParkingSpotPaths tilemapHeight parkingSpots =
    parkingSpots
        |> List.indexedMap
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
                parkingSpot.pathToLotExit |> List.map (flipSplineYCoordinate tilemapHeight >> cubicSpline color)
            )
        |> List.concat


type SVGCoordinates
    = SVGCoordinates -- Y down instead of up


flipSplineYCoordinate : Length -> LMCubicSpline2d -> CubicSpline2d Length.Meters SVGCoordinates
flipSplineYCoordinate tilemapHeight spline =
    let
        flipFn =
            flipPointYCoordinate tilemapHeight

        cp1 =
            CubicSpline2d.firstControlPoint spline |> flipFn

        cp2 =
            CubicSpline2d.secondControlPoint spline |> flipFn

        cp3 =
            CubicSpline2d.thirdControlPoint spline |> flipFn

        cp4 =
            CubicSpline2d.fourthControlPoint spline |> flipFn
    in
    CubicSpline2d.fromControlPoints cp1 cp2 cp3 cp4


flipPointYCoordinate : Length -> LMPoint2d -> Point2d Length.Meters SVGCoordinates
flipPointYCoordinate tilemapHeight originalPoint =
    let
        newY =
            tilemapHeight |> Quantity.minus (Point2d.yCoordinate originalPoint)
    in
    Point2d.xy
        (Point2d.xCoordinate originalPoint)
        newY


cubicSpline : Color -> CubicSpline2d Length.Meters SVGCoordinates -> Svg msg
cubicSpline color spline =
    let
        splinePixels =
            CubicSpline2d.at Render.Conversion.pixelsToMetersRatio spline

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
