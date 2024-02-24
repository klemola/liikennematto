module Common exposing
    ( andCarry
    , angleFromDirection
    , attemptFoldList
    , attemptMapList
    , boundingBoxOverlaps
    , boundingBoxToFrame
    , boundingBoxWithDimensions
    , isCloseToZeroVelocity
    , isInTheNormalPlaneOf
    , randomFutureTime
    , rightAnglePosition
    , splitBoundingBoxHorizontally
    , splitBoundingBoxVertically
    )

import Angle exposing (Angle)
import BoundingBox2d
import Direction2d exposing (Direction2d)
import Frame2d
import Length exposing (Length)
import Model.Geometry exposing (LMBoundingBox2d, LMFrame2d, LMPoint2d)
import Point2d exposing (Point2d)
import Quantity
import Random
import Speed exposing (Speed)
import Time
import Vector2d


angleFromDirection :
    Direction2d coordinates
    -> Point2d Length.Meters coordinates
    -> Point2d Length.Meters coordinates
    -> Angle
angleFromDirection direction target origin =
    Direction2d.from origin target
        |> Maybe.map (Direction2d.angleFrom direction)
        |> Maybe.withDefault (Angle.degrees 0)


rightAnglePosition :
    Point2d Length.Meters coordinates
    -> Point2d Length.Meters coordinates
    -> Direction2d coordinates
    -> Point2d Length.Meters coordinates
rightAnglePosition origin target direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        cosine =
            origin
                |> angleFromDirection direction target
                |> Angle.cos

        distanceToRightAnglePosition =
            distanceToTarget |> Quantity.multiplyBy cosine
    in
    Point2d.translateIn direction distanceToRightAnglePosition origin


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


boundingBoxToFrame : LMBoundingBox2d -> LMFrame2d
boundingBoxToFrame bb =
    let
        bbExtrema =
            BoundingBox2d.extrema bb

        pos =
            Point2d.xy bbExtrema.minX bbExtrema.minY
    in
    Frame2d.atPoint pos


isInTheNormalPlaneOf : Direction2d a -> Point2d Length.Meters a -> Point2d Length.Meters a -> Bool
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


isCloseToZeroVelocity : Speed -> Bool
isCloseToZeroVelocity =
    Quantity.abs >> Quantity.lessThan (Speed.metersPerSecond 0.1)


addMillisecondsToPosix : Int -> Time.Posix -> Time.Posix
addMillisecondsToPosix millis time =
    Time.posixToMillis time + millis |> Time.millisToPosix


randomFutureTime : ( Int, Int ) -> Time.Posix -> Random.Generator Time.Posix
randomFutureTime ( minDelay, maxDelay ) now =
    Random.int minDelay maxDelay
        |> Random.map (\delay -> addMillisecondsToPosix delay now)



--
-- Maybe utility
--


andCarry : (a -> Maybe b) -> Maybe a -> Maybe ( a, b )
andCarry nextFn carried =
    Maybe.map2 Tuple.pair
        carried
        (carried |> Maybe.andThen nextFn)



--
-- Result utility
--


attemptMapList : (a -> Result x b) -> List a -> Result x (List b)
attemptMapList fn list =
    case list of
        [] ->
            Ok []

        x :: xs ->
            case fn x of
                Err e ->
                    Err e

                Ok y ->
                    Result.map (\acc -> y :: acc) (attemptMapList fn xs)


attemptFoldList : (a -> b -> Result x b) -> b -> List a -> Result x b
attemptFoldList fn acc list =
    case list of
        [] ->
            Ok acc

        head :: tail ->
            case fn head acc of
                Ok result ->
                    case attemptFoldList fn result tail of
                        Ok results ->
                            Ok results

                        Err err ->
                            Err err

                Err err ->
                    Err err
