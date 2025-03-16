module Render.Shape exposing
    ( arc
    , boundingBox
    , circle
    , cubicSpline
    , cubicSplineDebug
    , line
    )

import Arc2d exposing (Arc2d)
import Axis2d
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d
import Color exposing (Color)
import Common exposing (GlobalCoordinates)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Colors as Colors
import Geometry.Svg as Svg
import Length
import LineSegment2d exposing (LineSegment2d)
import Model.RenderCache exposing (RenderCache)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type SVGCoordinates
    = SVGCoordinates -- Y down instead of up


cubicSpline : RenderCache -> Color -> CubicSpline2d Length.Meters GlobalCoordinates -> Svg msg
cubicSpline cache color spline =
    let
        splineInSVGCoords =
            flipSplineYCoordinate cache.tilemapHeight spline

        splinePixels =
            CubicSpline2d.at cache.pixelsToMetersRatio splineInSVGCoords
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
        , circle
            cache
            Colors.gray5
            Nothing
            (Length.meters 0.5)
            (CubicSpline2d.startPoint spline)
        , circle
            cache
            Colors.gray5
            Nothing
            (Length.meters 0.5)
            (CubicSpline2d.endPoint spline)
        ]


cubicSplineDebug : RenderCache -> Color -> CubicSpline2d Length.Meters GlobalCoordinates -> Svg msg
cubicSplineDebug cache color spline =
    let
        splineInSVGCoords =
            flipSplineYCoordinate cache.tilemapHeight spline

        splinePixels =
            CubicSpline2d.at cache.pixelsToMetersRatio splineInSVGCoords

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
                        (flipPointYCoordinate cache.tilemapHeight
                            >> Point2d.at cache.pixelsToMetersRatio
                        )
                )
            )
        , Svg.g [ Attributes.fill "rgba(255, 255, 255, 0.5)" ]
            (List.map
                (circle
                    cache
                    Colors.gray5
                    Nothing
                    (Length.meters 0.5)
                )
                controlPoints
            )
        ]


circle : RenderCache -> Color -> Maybe ( Color, Float ) -> Length.Length -> Point2d Length.Meters GlobalCoordinates -> Svg msg
circle cache fillColor strokeProperties radius centerPoint =
    let
        centerPointInSVGCoords =
            centerPoint
                |> flipPointYCoordinate cache.tilemapHeight
                |> Point2d.at cache.pixelsToMetersRatio

        radiusPixels =
            radius |> Quantity.at cache.pixelsToMetersRatio

        strokeAttrs =
            case strokeProperties of
                Just ( strokeColor, strokeWidth ) ->
                    [ Attributes.stroke (Color.toCssString strokeColor)
                    , Attributes.strokeWidth (String.fromFloat strokeWidth)
                    ]

                Nothing ->
                    []
    in
    Svg.circle2d
        (Attributes.fill (Color.toCssString fillColor) :: strokeAttrs)
        (Circle2d.atPoint centerPointInSVGCoords radiusPixels)


line : RenderCache -> Color -> LineSegment2d Length.Meters GlobalCoordinates -> Svg msg
line cache color lineSegment =
    let
        rayInSVGCoords =
            LineSegment2d.fromEndpoints
                ( LineSegment2d.startPoint lineSegment
                    |> flipPointYCoordinate cache.tilemapHeight
                    |> Point2d.at cache.pixelsToMetersRatio
                , LineSegment2d.endPoint lineSegment
                    |> flipPointYCoordinate cache.tilemapHeight
                    |> Point2d.at cache.pixelsToMetersRatio
                )
    in
    Svg.lineSegment2d
        [ Attributes.stroke (Color.toCssString color)
        , Attributes.strokeWidth "4"
        ]
        rayInSVGCoords


arc : RenderCache -> Color -> Arc2d Length.Meters GlobalCoordinates -> Svg msg
arc cache color theArc =
    let
        start =
            Arc2d.startPoint theArc
                |> flipPointYCoordinate cache.tilemapHeight
                |> Point2d.at cache.pixelsToMetersRatio

        center =
            Arc2d.centerPoint theArc
                |> flipPointYCoordinate cache.tilemapHeight
                |> Point2d.at cache.pixelsToMetersRatio

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


boundingBox : RenderCache -> Color -> BoundingBox2d Length.Meters GlobalCoordinates -> Svg msg
boundingBox cache color bb =
    let
        centerPointPixels =
            BoundingBox2d.centerPoint bb
                |> flipPointYCoordinate cache.tilemapHeight
                |> Point2d.at cache.pixelsToMetersRatio

        dimensionsPixels =
            BoundingBox2d.dimensions bb
                |> Tuple.mapBoth (Quantity.at cache.pixelsToMetersRatio)
                    (Quantity.at cache.pixelsToMetersRatio)
    in
    Svg.boundingBox2d
        [ Attributes.fill (Color.toCssString color) ]
        (BoundingBox2d.withDimensions dimensionsPixels centerPointPixels)



--
-- Conversion
--


flipSplineYCoordinate : Length.Length -> CubicSpline2d Length.Meters GlobalCoordinates -> CubicSpline2d Length.Meters SVGCoordinates
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


flipPointYCoordinate : Length.Length -> Point2d Length.Meters GlobalCoordinates -> Point2d Length.Meters SVGCoordinates
flipPointYCoordinate renderAreaHeight originalPoint =
    let
        newY =
            renderAreaHeight |> Quantity.minus (Point2d.yCoordinate originalPoint)
    in
    Point2d.xy
        (Point2d.xCoordinate originalPoint)
        newY
