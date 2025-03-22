module Common exposing
    ( GlobalCoordinates
    , LocalCoordinates
    , andCarry
    , angleFromDirection
    , applyTuple2
    , attemptFoldList
    , attemptMapList
    , boundingBoxToFrame
    , boundingBoxWithDimensions
    , isCloseToZeroVelocity
    , isInTheNormalPlaneOf
    , randomFutureTime
    , rightAnglePoint
    , splitBoundingBoxHorizontally
    , splitBoundingBoxVertically
    )

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity
import Random
import Speed exposing (Speed)
import Time
import Vector2d


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


angleFromDirection :
    Direction2d coordinates
    -> Point2d Length.Meters coordinates
    -> Point2d Length.Meters coordinates
    -> Angle
angleFromDirection direction target origin =
    Direction2d.from origin target
        |> Maybe.map (Direction2d.angleFrom direction)
        |> Maybe.withDefault (Angle.degrees 0)


rightAnglePoint :
    Point2d Length.Meters coordinates
    -> Point2d Length.Meters coordinates
    -> Direction2d coordinates
    -> Point2d Length.Meters coordinates
rightAnglePoint origin target direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        cosine =
            origin
                |> angleFromDirection direction target
                |> Angle.cos

        distanceToRightAnglePoint =
            distanceToTarget |> Quantity.multiplyBy cosine
    in
    Point2d.translateIn direction distanceToRightAnglePoint origin


boundingBoxWithDimensions : Length -> Length -> Point2d Length.Meters GlobalCoordinates -> BoundingBox2d Length.Meters GlobalCoordinates
boundingBoxWithDimensions width height origin =
    let
        otherCorner =
            origin
                |> Point2d.translateIn Direction2d.positiveX width
                |> Point2d.translateIn Direction2d.positiveY height
    in
    BoundingBox2d.from origin otherCorner


splitBoundingBoxHorizontally :
    BoundingBox2d Length.Meters GlobalCoordinates
    -> { left : BoundingBox2d Length.Meters GlobalCoordinates, right : BoundingBox2d Length.Meters GlobalCoordinates }
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


boundingBoxLeftHalf : BoundingBox2d Length.Meters GlobalCoordinates -> BoundingBox2d Length.Meters GlobalCoordinates
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


splitBoundingBoxVertically :
    BoundingBox2d Length.Meters GlobalCoordinates
    -> { lower : BoundingBox2d Length.Meters GlobalCoordinates, upper : BoundingBox2d Length.Meters GlobalCoordinates }
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


boundingBoxLowerHalf : BoundingBox2d Length.Meters GlobalCoordinates -> BoundingBox2d Length.Meters GlobalCoordinates
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


boundingBoxToFrame :
    BoundingBox2d Length.Meters GlobalCoordinates
    -> Frame2d Length.Meters GlobalCoordinates { defines : LocalCoordinates }
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



--
-- Tuple utility
--


applyTuple2 : (a -> b -> c) -> ( a, b ) -> c
applyTuple2 fn ( a, b ) =
    fn a b
