module Simulation.Collision exposing
    ( CollisionCheckResult(..)
    , checkFutureCollision
    , fieldOfViewCheck
    , maxCarCollisionTestDistance
    , pathRay
    , rightSideFOV
    )

import Angle exposing (Angle)
import Arc2d
import Common
import Direction2d
import Length exposing (Length)
import LineSegment2d
import Model.Geometry
    exposing
        ( LMArc2d
        , LMDirection2d
        , LMLineSegment2d
        , LMPoint2d
        , LMShape2d
        )
import Point2d
import Polygon2d
import Quantity
import Simulation.Car as Car exposing (Car)
import Simulation.Route as Route
import Speed


type CollisionCheckResult
    = PotentialCollision LMPoint2d
    | Caution
    | NoCollision


maxCarCollisionTestDistance : Length
maxCarCollisionTestDistance =
    Length.meters 12


pathLookAheadDistance : Length
pathLookAheadDistance =
    Length.meters 3.2


maxSimilarDirectionDifference : Angle
maxSimilarDirectionDifference =
    Angle.radians 0.1


fovRadius : Angle
fovRadius =
    Angle.degrees 90


checkFutureCollision : Car -> Car -> CollisionCheckResult
checkFutureCollision activeCar otherCar =
    if Car.currentState activeCar == Car.Parked then
        NoCollision

    else
        let
            ray =
                pathRay activeCar maxCarCollisionTestDistance
        in
        case rayCollisionAt activeCar.position otherCar.shape ray of
            Just collisionPoint ->
                PotentialCollision collisionPoint

            Nothing ->
                -- No ray collision, check FOV
                if shouldCheckFov activeCar otherCar && fieldOfViewCheck (rightSideFOV ray) otherCar.position then
                    Caution

                else
                    NoCollision


rayCollisionAt : LMPoint2d -> LMShape2d -> LMLineSegment2d -> Maybe LMPoint2d
rayCollisionAt origin shape ray =
    Polygon2d.edges shape
        |> List.filterMap (LineSegment2d.intersectionPoint ray)
        |> List.sortWith
            (\pt1 pt2 ->
                Quantity.compare
                    (Point2d.distanceFrom origin pt1)
                    (Point2d.distanceFrom origin pt2)
            )
        |> List.head


shouldCheckFov : Car -> Car -> Bool
shouldCheckFov activeCar otherCar =
    Car.shouldWatchTraffic activeCar
        && (activeCar.velocity |> Quantity.greaterThanZero)
        && (otherCar.velocity |> Quantity.greaterThan (Quantity.half activeCar.velocity))
        && (activeCar.position
                |> Common.isInTheNormalPlaneOf
                    (Direction2d.fromAngle otherCar.orientation)
                    otherCar.position
           )


fieldOfViewCheck : LMArc2d -> LMPoint2d -> Bool
fieldOfViewCheck fieldOfView target =
    let
        origin =
            Arc2d.centerPoint fieldOfView

        directionToTarget =
            Direction2d.from origin target

        directionToArcMidPoint =
            Direction2d.from origin (Arc2d.midpoint fieldOfView)
    in
    Maybe.map2
        (\targetDirection arcMidPointDirection ->
            let
                distanceToTarget =
                    Point2d.distanceFrom origin target
            in
            (distanceToTarget |> Quantity.lessThanOrEqualTo (Arc2d.radius fieldOfView))
                && Direction2d.equalWithin (Quantity.half fovRadius) targetDirection arcMidPointDirection
        )
        directionToTarget
        directionToArcMidPoint
        |> Maybe.withDefault False


rightSideFOV : LMLineSegment2d -> LMArc2d
rightSideFOV ray =
    LineSegment2d.endPoint ray
        |> Arc2d.sweptAround (LineSegment2d.startPoint ray)
            (Quantity.negate fovRadius)


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
                case Route.sampleAhead car.route pathLookAheadDistance of
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
                Quantity.multiplyBy 0.6 maxDistance
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
