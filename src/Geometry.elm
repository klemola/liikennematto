module Geometry exposing
    ( LMBoundingBox2d
    , LMCircle2d
    , LMCubicSpline2d
    , LMDirection2d
    , LMPoint2d
    , LMTriangle2d
    , LocalPath
    , accelerationToString
    , angleFromDirection
    , angleToTarget
    , boundingBoxFromCircle
    , boundingBoxWithDimensions
    , curveSpline
    , fieldOfViewTriangle
    , leaveLotSpline
    , linearLocalPathToTarget
    , noBoundingBoxOverlap
    , pathsCouldCollide
    , pointToString
    , speedToString
    , uTurnSpline
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import Config
    exposing
        ( lotExitOffset
        , overlapThreshold
        , pixelsToMetersRatio
        , uTurnDistance
        )
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Length exposing (Length, Meters)
import LineSegment2d
import Maybe.Extra
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Meters LMEntityCoordinates


type alias LMPolyline2d =
    Polyline2d Meters LMEntityCoordinates


type alias LMTriangle2d =
    Triangle2d Meters LMEntityCoordinates


type alias LMCircle2d =
    Circle2d Meters LMEntityCoordinates


type alias LMCubicSpline2d =
    CubicSpline2d Meters LMEntityCoordinates



-- Conversion


speedToString : Speed -> String
speedToString speed =
    let
        speedValue =
            speed
                |> Quantity.unwrap
                |> String.fromFloat
    in
    "Speed: " ++ speedValue ++ " m/s"


accelerationToString : Acceleration -> String
accelerationToString acceleration =
    let
        accelerationValue =
            acceleration
                |> Quantity.unwrap
                |> String.fromFloat
    in
    "Acceleration: " ++ accelerationValue ++ " m/s²"


pointToString : LMPoint2d -> String
pointToString point =
    let
        { x, y } =
            point
                |> Point2d.at pixelsToMetersRatio
                |> Point2d.toPixels

        format n =
            n
                |> truncate
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format x
        , "y:"
        , format y
        ]



-- Angles


angleToTarget : LMPoint2d -> LMPoint2d -> Angle
angleToTarget target origin =
    Direction2d.from origin target
        |> Maybe.map Direction2d.toAngle
        |> Maybe.withDefault (Angle.degrees 0)


angleFromDirection : LMDirection2d -> LMPoint2d -> LMPoint2d -> Angle
angleFromDirection direction target origin =
    Direction2d.from origin target
        |> Maybe.map (Direction2d.angleFrom direction)
        |> Maybe.withDefault (Angle.degrees 0)



-- Bounding boxes


boundingBoxWithDimensions : Length -> Length -> LMPoint2d -> LMBoundingBox2d
boundingBoxWithDimensions width height origin =
    let
        otherCorner =
            origin
                |> Point2d.translateIn Direction2d.positiveX width
                |> Point2d.translateIn Direction2d.positiveY height
    in
    BoundingBox2d.from origin otherCorner


noBoundingBoxOverlap : LMBoundingBox2d -> LMBoundingBox2d -> Bool
noBoundingBoxOverlap bb1 bb2 =
    not <| BoundingBox2d.overlappingByAtLeast overlapThreshold bb1 bb2


boundingBoxFromCircle : LMPoint2d -> Length -> LMBoundingBox2d
boundingBoxFromCircle position radius =
    Circle2d.atPoint position radius |> Circle2d.boundingBox



-- Triangles and field of view


fieldOfViewTriangle : LMPoint2d -> LMDirection2d -> Angle -> Length -> LMTriangle2d
fieldOfViewTriangle origin direction fov distance =
    let
        leftVertexDirection =
            Direction2d.rotateBy fov direction

        rightVertexAngle =
            Quantity.minus fov (Angle.degrees 360)

        rightVertexDirection =
            Direction2d.rotateBy rightVertexAngle direction

        farLeftVertex =
            origin |> Point2d.translateIn leftVertexDirection distance

        farRightVertex =
            origin |> Point2d.translateIn rightVertexDirection distance
    in
    Triangle2d.fromVertices ( origin, farLeftVertex, farRightVertex )



-- Local path is a list of points, often constructed from a spline. Practically same as a Polyline2d.


type alias LocalPath =
    List LMPoint2d


splineSegmentsAmount : Int
splineSegmentsAmount =
    16


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
                |> angleFromDirection direction target
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


cubicSplineToLocalPath : LMCubicSpline2d -> LocalPath
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


pathsOverlap : LMPolyline2d -> LMPolyline2d -> Bool
pathsOverlap path1 path2 =
    Maybe.map2 BoundingBox2d.intersects
        (Polyline2d.boundingBox path1)
        (Polyline2d.boundingBox path2)
        |> Maybe.withDefault False


pathsIntersect : LMPolyline2d -> LMPolyline2d -> Bool
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
