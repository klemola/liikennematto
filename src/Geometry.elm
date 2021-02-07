module Geometry exposing
    ( LMBoundingBox2d
    , LMCubicSpline2d
    , LMDirection2d
    , LMEntityPositionUnitless
    , LMPoint2d
    , LocalPath
    , angleFromDirection
    , angleToTarget
    , boundingBoxWithDimensions
    , curveSpline
    , isPointAt
    , leaveLotSpline
    , linearLocalPathToTarget
    , pointFromPosition
    , pointToPosition
    , pointToPositionAsTuple
    , pointToString
    , toFloat
    , toLMUnits
    , translateBoundingBoxIn
    , translatePointBy
    , translatePointIn
    , uTurnSpline
    )

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Config exposing (tileSize)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity exposing (Quantity)
import Vector2d


type alias LMEntityCoordinates =
    ()


type alias LMEntityUnits =
    Pixels


type alias LMPoint2d =
    Point2d LMEntityUnits LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d LMEntityUnits LMEntityCoordinates


type alias LMCubicSpline2d =
    CubicSpline2d LMEntityUnits LMEntityCoordinates


type alias LMEntityPositionUnitless =
    { x : Float, y : Float }


translatePointBy : Float -> Float -> LMPoint2d -> LMPoint2d
translatePointBy x y =
    Point2d.translateBy (Vector2d.pixels x y)


translatePointIn : LMDirection2d -> Float -> LMPoint2d -> LMPoint2d
translatePointIn direction amount =
    Point2d.translateIn direction (Pixels.pixels amount)


pointToPosition : LMPoint2d -> LMEntityPositionUnitless
pointToPosition =
    Point2d.toPixels


pointToPositionAsTuple : LMPoint2d -> ( Float, Float )
pointToPositionAsTuple =
    Point2d.toTuple Pixels.inPixels


pointToString : LMPoint2d -> String
pointToString point =
    let
        { x, y } =
            pointToPosition point

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


pointFromPosition : LMEntityPositionUnitless -> LMPoint2d
pointFromPosition { x, y } =
    Point2d.pixels x y


translateBoundingBoxIn : LMDirection2d -> Float -> LMBoundingBox2d -> LMBoundingBox2d
translateBoundingBoxIn direction amount =
    BoundingBox2d.translateIn direction (Pixels.pixels amount)


toFloat : Quantity Float LMEntityUnits -> Float
toFloat =
    Pixels.toFloat


toLMUnits : Float -> Quantity Float LMEntityUnits
toLMUnits val =
    Pixels.pixels val


isPointAt : LMPoint2d -> LMPoint2d -> Bool
isPointAt target origin =
    Point2d.equalWithin (Pixels.pixels 1) origin target


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


boundingBoxWithDimensions : Float -> Float -> LMPoint2d -> LMBoundingBox2d
boundingBoxWithDimensions width height origin =
    let
        otherCorner =
            origin
                |> translatePointIn Direction2d.positiveX width
                |> translatePointIn Direction2d.positiveY height
    in
    BoundingBox2d.from origin otherCorner



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
            Point2d.distanceFrom origin target
                |> Quantity.half

        targetCp =
            target
                |> translatePointIn direction (tileSize / 2)

        midpoint =
            Point2d.midpoint origin targetCp

        handleCp1 =
            midpoint
                |> Point2d.midpoint origin
                |> Point2d.translateIn (Direction2d.rotateClockwise direction) handleDistance

        handleCp2 =
            Point2d.translateIn (Direction2d.rotateCounterclockwise direction) handleDistance midpoint
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> cubicSplineToLocalPath


uTurnSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
uTurnSpline origin target direction =
    let
        turnDistance =
            toLMUnits (tileSize / 4)

        handleCp1 =
            Point2d.translateIn direction turnDistance origin

        handleCp2 =
            Point2d.translateIn direction turnDistance target
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
