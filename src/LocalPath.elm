module LocalPath exposing (..)

import Angle
import Axis2d exposing (direction)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d
import Geometry exposing (LMDirection2d, LMEntityCoordinates, LMPoint2d)
import Length exposing (Length)
import Point2d
import Polyline2d
import Quantity
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)



-- Local path is a list of points, often constructed from a spline. Practically same as a Polyline2d.


type alias LocalPath =
    List LMPoint2d


splineSegmentsAmount : Int
splineSegmentsAmount =
    8


lotExitOffset : Length
lotExitOffset =
    Length.meters 10


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


type alias PathParameters =
    { origin : LMPoint2d
    , direction : LMDirection2d
    , useOffsetSpline : Bool
    }


toNode : PathParameters -> RNNodeContext -> LocalPath
toNode { direction, origin, useOffsetSpline } { node } =
    let
        target =
            node.label.position

        angleDegreesToTarget =
            origin
                |> Geometry.angleFromDirection direction target
                |> Quantity.abs
    in
    if node.label.kind == LotEntry && useOffsetSpline then
        offsetSpline origin target direction

    else if node.label.kind == DeadendExit then
        uTurnSpline origin target direction

    else if angleDegreesToTarget |> Quantity.lessThan (Angle.radians 0.1) then
        [ origin, target ]

    else
        curveSpline origin target direction


isWithinRoundingErrorOf : Int -> Float -> Bool
isWithinRoundingErrorOf target value =
    round value == target || floor value == target


offsetSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
offsetSpline origin target direction =
    let
        targetCp =
            target |> Point2d.translateIn direction lotExitOffset

        distanceToTarget =
            Point2d.distanceFrom origin targetCp

        handleCp1 =
            origin
                |> Point2d.translateIn
                    direction
                    (distanceToTarget |> Quantity.multiplyBy 0.25)

        handleCp2 =
            targetCp
                |> Point2d.translateIn
                    (Direction2d.reverse direction)
                    (distanceToTarget |> Quantity.half)
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> cubicSplineToLocalPath


uTurnSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
uTurnSpline origin target direction =
    let
        handleCp1 =
            Point2d.translateIn direction uTurnDistance origin

        handleCp2 =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> cubicSplineToLocalPath


curveSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
curveSpline origin target direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        cosine =
            origin
                |> Geometry.angleFromDirection direction target
                |> Angle.cos

        distanceToCorner =
            Quantity.multiplyBy cosine distanceToTarget

        corner =
            Point2d.translateIn direction distanceToCorner origin

        handleCp1 =
            Point2d.midpoint origin corner

        handleCp2 =
            Point2d.midpoint corner target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> cubicSplineToLocalPath


cubicSplineToLocalPath : CubicSpline2d Length.Meters LMEntityCoordinates -> LocalPath
cubicSplineToLocalPath spline =
    spline
        |> CubicSpline2d.segments splineSegmentsAmount
        |> Polyline2d.vertices
        -- Skip the first vertex
        |> List.tail
        |> Maybe.withDefault []
