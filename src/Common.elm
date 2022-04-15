module Common exposing
    ( angleFromDirection
    , boundingBoxOverlaps
    , boundingBoxToFrame
    , boundingBoxWithDimensions
    , isInTheNormalPlaneOf
    , splitBoundingBoxHorizontally
    , splitBoundingBoxVertically
    )

import Angle exposing (Angle)
import BoundingBox2d
import Direction2d
import Frame2d
import Length exposing (Length)
import Model.Geometry exposing (LMBoundingBox2d, LMDirection2d, LMPoint2d)
import Point2d
import Quantity
import Vector2d


angleFromDirection : LMDirection2d -> LMPoint2d -> LMPoint2d -> Angle
angleFromDirection direction target origin =
    Direction2d.from origin target
        |> Maybe.map (Direction2d.angleFrom direction)
        |> Maybe.withDefault (Angle.degrees 0)


boundingBoxWithDimensions : Length -> Length -> LMPoint2d -> LMBoundingBox2d
boundingBoxWithDimensions width height origin =
    let
        otherCorner =
            origin
                |> Point2d.translateIn Direction2d.positiveX width
                |> Point2d.translateIn Direction2d.positiveY height
    in
    BoundingBox2d.from origin otherCorner


boundingBoxOverlaps : LMBoundingBox2d -> LMBoundingBox2d -> Bool
boundingBoxOverlaps bb1 bb2 =
    BoundingBox2d.overlappingByAtLeast (Length.meters 0.1) bb1 bb2


splitBoundingBoxHorizontally : LMBoundingBox2d -> { left : LMBoundingBox2d, right : LMBoundingBox2d }
splitBoundingBoxHorizontally bb =
    let
        leftHalf =
            boundingBoxLeftHalf bb

        ( width, _ ) =
            BoundingBox2d.dimensions leftHalf

        rightHalf =
            BoundingBox2d.translateIn Direction2d.positiveX width leftHalf
    in
    { left = leftHalf
    , right = rightHalf
    }


boundingBoxLeftHalf : LMBoundingBox2d -> LMBoundingBox2d
boundingBoxLeftHalf bb =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bb

        width =
            maxX |> Quantity.minus minX

        x =
            width |> Quantity.half |> Quantity.plus minX
    in
    BoundingBox2d.fromExtrema
        { minX = minX
        , minY = minY
        , maxX = x
        , maxY = maxY
        }


splitBoundingBoxVertically : LMBoundingBox2d -> { lower : LMBoundingBox2d, upper : LMBoundingBox2d }
splitBoundingBoxVertically bb =
    let
        lowerHalf =
            boundingBoxLowerHalf bb

        ( _, heigth ) =
            BoundingBox2d.dimensions lowerHalf

        upperHalf =
            BoundingBox2d.translateIn Direction2d.positiveY heigth lowerHalf
    in
    { lower = lowerHalf
    , upper = upperHalf
    }


boundingBoxLowerHalf : LMBoundingBox2d -> LMBoundingBox2d
boundingBoxLowerHalf bb =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bb

        height =
            maxY |> Quantity.minus minY

        y =
            height |> Quantity.half |> Quantity.plus minY
    in
    BoundingBox2d.fromExtrema
        { minX = minX
        , minY = minY
        , maxX = maxX
        , maxY = y
        }


boundingBoxToFrame bb =
    let
        bbExtrema =
            BoundingBox2d.extrema bb

        pos =
            Point2d.xy bbExtrema.minX bbExtrema.minY
    in
    Frame2d.atPoint pos


isInTheNormalPlaneOf : LMDirection2d -> LMPoint2d -> LMPoint2d -> Bool
isInTheNormalPlaneOf normal origin other =
    let
        p =
            Vector2d.fromMeters (Point2d.toMeters origin)

        a =
            Vector2d.fromMeters (Point2d.toMeters other)
    in
    Direction2d.toVector normal
        |> Vector2d.dot (a |> Vector2d.minus p)
        |> Quantity.greaterThanOrEqualToZero
