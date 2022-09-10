module Render.Shape exposing
    ( arc
    , boundingBox
    , circle
    , cubicSpline
    , cubicSplineDebug
    , line
    )

import Arc2d
import Axis2d
import BoundingBox2d
import Circle2d
import Color exposing (Color)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Colors as Colors
import Geometry.Svg as Svg
import Length
import LineSegment2d
import Model.Geometry exposing (LMArc2d, LMBoundingBox2d, LMCubicSpline2d, LMLineSegment2d, LMPoint2d)
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
    in
    Svg.g
        [ Attributes.stroke (Color.toCssString color)
        ]
        [ Svg.cubicSpline2d
            [ Attributes.strokeWidth "4"
            , Attributes.strokeLinecap "round"
            , Attributes.stroke (Color.toCssString color)
            , Attributes.fill "none"
            ]
            splinePixels
        , circle Colors.gray5 renderAreaHeight (Length.meters 0.5) (CubicSpline2d.startPoint spline)
        , circle Colors.gray5 renderAreaHeight (Length.meters 0.5) (CubicSpline2d.endPoint spline)
        ]


cubicSplineDebug : Color -> Length.Length -> LMCubicSpline2d -> Svg msg
cubicSplineDebug color renderAreaHeight spline =
    let
        splineInSVGCoords =
            flipSplineYCoordinate renderAreaHeight spline

        splinePixels =
            CubicSpline2d.at Render.Conversion.pixelsToMetersRatio splineInSVGCoords

        controlPoints =
            [ CubicSpline2d.firstControlPoint spline
            , CubicSpline2d.secondControlPoint spline
            , CubicSpline2d.thirdControlPoint spline
            , CubicSpline2d.fourthControlPoint spline
            ]
    in
    Svg.g
        [ Attributes.stroke (Color.toCssString color)
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
            (Polyline2d.fromVertices
                (controlPoints
                    |> List.map
                        (flipPointYCoordinate renderAreaHeight
                            >> Point2d.at Render.Conversion.pixelsToMetersRatio
                        )
                )
            )
        , Svg.g [ Attributes.fill "rgba(255, 255, 255, 0.5)" ]
            (List.map (circle Colors.gray5 renderAreaHeight (Length.meters 0.5)) controlPoints)
        ]


circle : Color -> Length.Length -> Length.Length -> LMPoint2d -> Svg msg
circle color renderAreaHeight radius centerPoint =
    let
        centerPointInSVGCoords =
            centerPoint
                |> flipPointYCoordinate renderAreaHeight
                |> Point2d.at Render.Conversion.pixelsToMetersRatio

        radiusPixels =
            radius |> Quantity.at Render.Conversion.pixelsToMetersRatio
    in
    Svg.circle2d
        [ Attributes.fill (Color.toCssString color)
        ]
        (Circle2d.atPoint centerPointInSVGCoords radiusPixels)


line : Color -> Length.Length -> LMLineSegment2d -> Svg msg
line color renderAreaHeight lineSegment =
    let
        rayInSVGCoords =
            LineSegment2d.fromEndpoints
                ( LineSegment2d.startPoint lineSegment
                    |> flipPointYCoordinate renderAreaHeight
                    |> Point2d.at Render.Conversion.pixelsToMetersRatio
                , LineSegment2d.endPoint lineSegment
                    |> flipPointYCoordinate renderAreaHeight
                    |> Point2d.at Render.Conversion.pixelsToMetersRatio
                )
    in
    Svg.lineSegment2d
        [ Attributes.stroke (Color.toCssString color)
        , Attributes.strokeWidth "4"
        ]
        rayInSVGCoords


arc : Color -> Length.Length -> LMArc2d -> Svg msg
arc color renderAreaHeight theArc =
    let
        start =
            Arc2d.startPoint theArc
                |> flipPointYCoordinate renderAreaHeight
                |> Point2d.at Render.Conversion.pixelsToMetersRatio

        center =
            Arc2d.centerPoint theArc
                |> flipPointYCoordinate renderAreaHeight
                |> Point2d.at Render.Conversion.pixelsToMetersRatio

        arcInSVGCoords =
            start
                |> Arc2d.sweptAround
                    center
                    (Arc2d.sweptAngle (theArc |> Arc2d.mirrorAcross Axis2d.x))
    in
    Svg.arc2d
        [ Attributes.stroke (Color.toCssString color)
        , Attributes.strokeWidth "4"
        , Attributes.fill "none"
        ]
        arcInSVGCoords


boundingBox : Color -> Length.Length -> LMBoundingBox2d -> Svg msg
boundingBox color renderAreaHeight bb =
    let
        centerPointPixels =
            BoundingBox2d.centerPoint bb
                |> flipPointYCoordinate renderAreaHeight
                |> Point2d.at Render.Conversion.pixelsToMetersRatio

        dimensionsPixels =
            BoundingBox2d.dimensions bb
                |> Tuple.mapBoth (Quantity.at Render.Conversion.pixelsToMetersRatio)
                    (Quantity.at Render.Conversion.pixelsToMetersRatio)
    in
    Svg.boundingBox2d
        [ Attributes.fill (Color.toCssString color) ]
        (BoundingBox2d.withDimensions dimensionsPixels centerPointPixels)



--
-- Conversion
--


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
