module Simulation.Collision exposing
    ( checkFutureCollision
    , fieldOfViewCheck
    , maxCarCollisionTestDistance
    , pathRay
    , pathsIntersect
    , rightSideFOV
    )

import Angle exposing (Angle)
import Arc2d
import Common
import Direction2d
import Length exposing (Length)
import LineSegment2d
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Geometry
    exposing
        ( LMArc2d
        , LMDirection2d
        , LMLineSegment2d
        , LMPoint2d
        , LMShape2d
        , LMTriangle2d
        )
import Model.Route as Route
import Point2d
import Polygon2d
import Quantity
import Speed
import Triangle2d


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


checkFutureCollision : Car -> Car -> Maybe LMPoint2d
checkFutureCollision activeCar otherCar =
    if
        let
            headingRoughlyInTheSameDirection =
                Quantity.equalWithin maxSimilarDirectionDifference activeCar.orientation otherCar.orientation

            canCatchUp =
                otherCar.velocity |> Quantity.lessThan activeCar.velocity
        in
        -- Avoid stuttering movement when the car is behind another and aligned
        (headingRoughlyInTheSameDirection && not canCatchUp)
            -- Ignore collision detection when parked (assume zero velocity)
            || Car.isParked activeCar
    then
        Nothing

    else
        let
            ray =
                pathRay activeCar maxCarCollisionTestDistance
        in
        Maybe.orLazy
            (rayCollisionAt activeCar.position otherCar.shape ray)
            -- No ray collision, check FOV
            (\_ ->
                if shouldCheckFov activeCar otherCar then
                    fieldOfViewCheck otherCar.position (rightSideFOV ray)

                else
                    Nothing
            )


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
        && not (Car.isStoppedOrWaiting otherCar)
        && (activeCar.position
                |> Common.isInTheNormalPlaneOf
                    (Direction2d.fromAngle otherCar.orientation)
                    otherCar.position
           )


fieldOfViewCheck : LMPoint2d -> LMArc2d -> Maybe LMPoint2d
fieldOfViewCheck target fieldOfView =
    let
        origin =
            Arc2d.centerPoint fieldOfView

        directionToTarget =
            Direction2d.from origin target

        directionToArcMidPoint =
            Direction2d.from origin (Arc2d.midpoint fieldOfView)
    in
    Maybe.andThen2
        (\targetDirection arcMidPointDirection ->
            let
                distanceToTarget =
                    Point2d.distanceFrom origin target
            in
            if distanceToTarget |> Quantity.greaterThan (Arc2d.radius fieldOfView) then
                Nothing

            else if Direction2d.equalWithin (Quantity.half fovRadius) targetDirection arcMidPointDirection then
                Just
                    -- The midpoint between origin and target
                    -- TODO: check if this a valid heuristic
                    (Point2d.translateIn
                        targetDirection
                        (Quantity.half distanceToTarget)
                        origin
                    )

            else
                Nothing
        )
        directionToTarget
        directionToArcMidPoint


rightSideFOV : LMLineSegment2d -> LMArc2d
rightSideFOV ray =
    LineSegment2d.endPoint ray
        |> Arc2d.sweptAround (LineSegment2d.startPoint ray)
            (Quantity.negate fovRadius)


pathsIntersect : LMTriangle2d -> Car -> Car -> Bool
pathsIntersect checkArea car otherCar =
    case pathsIntersectAt checkArea car otherCar of
        Just _ ->
            True

        Nothing ->
            False


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
