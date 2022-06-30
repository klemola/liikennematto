module Render.Shape exposing (cubicSpline)

import Circle2d
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Geometry.Svg as Svg
import Length
import Model.Geometry exposing (LMCubicSpline2d, LMPoint2d)
import Pixels
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Render.Conversion
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type SVGCoordinates
    = SVGCoordinates -- Y down instead of up


cubicSpline : Color -> Length.Length -> LMCubicSpline2d -> Svg msg
cubicSpline color renderAreaHeight spline =
    let
        splineInSVGCoords =
            flipSplineYCoordinate renderAreaHeight spline

        splinePixels =
            CubicSpline2d.at Render.Conversion.pixelsToMetersRatio splineInSVGCoords

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


flipSplineYCoordinate : Length.Length -> LMCubicSpline2d -> CubicSpline2d Length.Meters SVGCoordinates
flipSplineYCoordinate renderAreaHeight spline =
    let
        cp1 =
            CubicSpline2d.firstControlPoint spline |> flipPointYCoordinate renderAreaHeight

        cp2 =
            CubicSpline2d.secondControlPoint spline |> flipPointYCoordinate renderAreaHeight

        cp3 =
            CubicSpline2d.thirdControlPoint spline |> flipPointYCoordinate renderAreaHeight

        cp4 =
            CubicSpline2d.fourthControlPoint spline |> flipPointYCoordinate renderAreaHeight
    in
    CubicSpline2d.fromControlPoints cp1 cp2 cp3 cp4


flipPointYCoordinate : Length.Length -> LMPoint2d -> Point2d Length.Meters SVGCoordinates
flipPointYCoordinate renderAreaHeight originalPoint =
    let
        newY =
            renderAreaHeight |> Quantity.minus (Point2d.yCoordinate originalPoint)
    in
    Point2d.xy
        (Point2d.xCoordinate originalPoint)
        newY
