module LocalPath exposing (..)

import Angle
import BoundingBox2d
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d
import Geometry exposing (LMDirection2d, LMEntityCoordinates, LMPoint2d)
import Length exposing (Length)
import LineSegment2d
import Maybe.Extra
import Point2d
import Polyline2d exposing (Polyline2d)
import Quantity



-- Local path is a list of points, often constructed from a spline. Practically same as a Polyline2d.


type alias LocalPath =
    List LMPoint2d


splineSegmentsAmount : Int
splineSegmentsAmount =
    16


lotExitOffset : Length
lotExitOffset =
    Length.meters 8


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


linearLocalPathToTarget : LMPoint2d -> LMPoint2d -> LocalPath
linearLocalPathToTarget origin target =
    [ 0, 0.25, 0.5, 0.75, 1 ]
        |> List.map (\step -> Point2d.interpolateFrom origin target step)


leaveLotSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
leaveLotSpline origin target direction =
    let
        handleDistance =
            Point2d.distanceFrom origin target |> Quantity.half

        targetCp =
            target |> Point2d.translateIn direction lotExitOffset

        midpoint =
            Point2d.midpoint origin targetCp

        handleCp1 =
            midpoint
                |> Point2d.midpoint origin
                |> Point2d.translateIn (Direction2d.rotateClockwise direction) handleDistance

        handleCp2 =
            midpoint |> Point2d.translateIn (Direction2d.rotateCounterclockwise direction) handleDistance
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


pathsCouldCollide : LocalPath -> LocalPath -> Bool
pathsCouldCollide path1 path2 =
    let
        -- Room for improvement: Lower the segments amount here to optimize
        path1AsPolyline =
            Polyline2d.fromVertices path1

        path2AsPolyline =
            Polyline2d.fromVertices path2
    in
    pathsOverlap path1AsPolyline path2AsPolyline
        && pathsIntersect path1AsPolyline path2AsPolyline


pathsOverlap : Polyline2d Length.Meters LMEntityCoordinates -> Polyline2d Length.Meters LMEntityCoordinates -> Bool
pathsOverlap path1 path2 =
    Maybe.map2 BoundingBox2d.intersects
        (Polyline2d.boundingBox path1)
        (Polyline2d.boundingBox path2)
        |> Maybe.withDefault False


pathsIntersect : Polyline2d Length.Meters LMEntityCoordinates -> Polyline2d Length.Meters LMEntityCoordinates -> Bool
pathsIntersect path1 path2 =
    let
        path1Segments =
            Polyline2d.segments path1

        path2Segments =
            Polyline2d.segments path2
    in
    path1Segments
        |> List.any
            (\segment ->
                path2Segments
                    |> List.any (LineSegment2d.intersectionPoint segment >> Maybe.Extra.isJust)
            )
