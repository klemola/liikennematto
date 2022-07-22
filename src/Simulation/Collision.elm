module Simulation.Collision exposing
    ( distanceToClosestCollisionPoint
    , forwardCollisionWith
    , pathCollisionWith
    , pathRay
    , pathsIntersectAt
    )

import Angle
import Direction2d
import Length exposing (Length)
import LineSegment2d
import Model.Car as Car exposing (Car)
import Model.Geometry
    exposing
        ( LMDirection2d
        , LMLineSegment2d
        , LMPoint2d
        , LMTriangle2d
        )
import Model.Route as Route
import Point2d
import Polygon2d
import Quantity
import Speed
import Triangle2d


maxSimilarDirectionDifference : Angle.Angle
maxSimilarDirectionDifference =
    Angle.radians 0.1


forwardCollisionWith : LMLineSegment2d -> Car -> Car -> Maybe LMPoint2d
forwardCollisionWith ray activeCar otherCar =
    let
        headingRoughlyInTheSameDirection =
            Quantity.equalWithin maxSimilarDirectionDifference activeCar.orientation otherCar.orientation

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
            (velocityRay car Car.viewDistance)
            (velocityRay otherCar Car.viewDistance)

    else
        Nothing


velocityRay : Car -> Length -> LMLineSegment2d
velocityRay car distance =
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
    buildRay
        origin
        directionOfVelocity
        distance


pathRay : Car -> Length -> LMLineSegment2d
pathRay car maxDistance =
    let
        origin =
            car.position

        carDirection =
            Direction2d.fromAngle car.orientation

        direction =
            -- If the car is reversing significantly, the ray should be behind the car
            if car.velocity |> Quantity.lessThan (Speed.metersPerSecond -0.5) then
                Direction2d.reverse carDirection

            else
                let
                    lookAhead =
                        Length.meters 4
                in
                case Route.sampleAhead car.route lookAhead of
                    Just ( _, tangentDirection ) ->
                        tangentDirection

                    Nothing ->
                        carDirection

        distance =
            if
                Direction2d.equalWithin
                    maxSimilarDirectionDifference
                    carDirection
                    direction
            then
                maxDistance

            else
                Quantity.half maxDistance
    in
    buildRay
        origin
        direction
        distance


buildRay : LMPoint2d -> LMDirection2d -> Length -> LMLineSegment2d
buildRay origin direction distance =
    LineSegment2d.from
        origin
        (origin |> Point2d.translateIn direction distance)
