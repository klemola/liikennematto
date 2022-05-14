module Simulation.Collision exposing
    ( distanceToClosestCollisionPoint
    , forwardCollisionWith
    , pathCollisionWith
    , pathsIntersectAt
    , toRay
    )

import Angle
import Direction2d
import Length exposing (Length)
import LineSegment2d
import Model.Car as Car exposing (Car)
import Model.Geometry
    exposing
        ( LMLineSegment2d
        , LMPoint2d
        , LMTriangle2d
        )
import Point2d
import Polygon2d
import Quantity
import Triangle2d


forwardCollisionWith : LMLineSegment2d -> Car -> Car -> Maybe LMPoint2d
forwardCollisionWith ray activeCar otherCar =
    let
        maxOrientationDifference =
            Angle.degrees 5

        headingRoughlyInTheSameDirection =
            Quantity.equalWithin maxOrientationDifference activeCar.orientation otherCar.orientation

        canCatchUp =
            otherCar.velocity |> Quantity.lessThan activeCar.velocity
    in
    -- Avoid stuttering movement when the car is behind another and aligned
    if headingRoughlyInTheSameDirection && not canCatchUp then
        Nothing

    else
        Polygon2d.edges otherCar.shape
            |> List.filterMap (LineSegment2d.intersectionPoint ray)
            |> List.sortWith
                (\pt1 pt2 ->
                    Quantity.compare
                        (Point2d.distanceFrom activeCar.position pt1)
                        (Point2d.distanceFrom activeCar.position pt2)
                )
            |> List.head


pathCollisionWith : LMTriangle2d -> Car -> Car -> Maybe LMPoint2d
pathCollisionWith fieldOfViewTriangle activeCar otherCar =
    let
        maxOrientationDifference =
            Angle.degrees 10

        headingRoughlyInTheSameDirection =
            Quantity.equalWithin maxOrientationDifference activeCar.orientation otherCar.orientation
    in
    -- Avoid false positives for cars that are roughly aligned & optimize by ignoring stationary cars
    if Car.isStoppedOrWaiting otherCar || headingRoughlyInTheSameDirection then
        Nothing

    else
        -- Room for improvement: project the cars w.rt their velocity and see if they will later collide
        pathsIntersectAt fieldOfViewTriangle activeCar otherCar


distanceToClosestCollisionPoint : Car -> List Car -> (Car -> Maybe LMPoint2d) -> Maybe Length
distanceToClosestCollisionPoint activeCar carsToCheck predicate =
    carsToCheck
        |> List.filterMap (predicate >> Maybe.map (Point2d.distanceFrom activeCar.position))
        |> Quantity.minimum


pathsIntersectAt : LMTriangle2d -> Car -> Car -> Maybe LMPoint2d
pathsIntersectAt checkArea car otherCar =
    if Triangle2d.contains otherCar.position checkArea then
        LineSegment2d.intersectionPoint
            (toRay car Car.viewDistance)
            (toRay otherCar Car.viewDistance)

    else
        Nothing


toRay : Car -> Length -> LMLineSegment2d
toRay car distance =
    let
        origin =
            car.position

        carDirection =
            Direction2d.fromAngle car.orientation

        directionOfVelocity =
            if Quantity.lessThanZero car.velocity then
                Direction2d.reverse carDirection

            else
                carDirection
    in
    LineSegment2d.from
        origin
        (origin |> Point2d.translateIn directionOfVelocity distance)
