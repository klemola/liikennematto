module Simulation.Steering exposing
    ( Steering
    , accelerate
    , align
    , clampVelocity
    , goSlow
    , maxVelocity
    , none
    , reactToCollision
    , seekAndFaceTarget
    , stop
    , stopAtDistance
    , stopAtPathEnd
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import AngularAcceleration exposing (AngularAcceleration)
import AngularSpeed exposing (AngularSpeed)
import Duration
import Length exposing (Length)
import Model.Geometry exposing (LMPoint2d)
import Model.Route as Route exposing (Route)
import Point2d
import Quantity exposing (Quantity(..))
import Speed exposing (Speed)


type alias Steering =
    { linear : Maybe Acceleration
    , angular : Maybe AngularAcceleration
    }



--
-- Constants
--


minVelocity : Speed
minVelocity =
    Speed.metersPerSecond -5


maxVelocity : Speed
maxVelocity =
    Speed.metersPerSecond 11.1


maxAcceleration : Acceleration
maxAcceleration =
    Acceleration.metersPerSecondSquared 5


maxDeceleration : Acceleration
maxDeceleration =
    Acceleration.metersPerSecondSquared -20


maxAngularAcceleration : AngularAcceleration
maxAngularAcceleration =
    AngularAcceleration.radiansPerSecondSquared 22.3


maxRotation : AngularSpeed
maxRotation =
    AngularSpeed.radiansPerSecond 1



--
-- Steering behaviors
--


seekAndFaceTarget :
    { currentRotation : AngularSpeed
    , currentOrientation : Angle
    , targetOrientation : Angle
    }
    -> Steering
seekAndFaceTarget alignProps =
    let
        alignSteering =
            align alignProps

        accelerationForRotation angularSteering =
            let
                rawAngular =
                    AngularAcceleration.inRadiansPerSecondSquared angularSteering
            in
            Acceleration.metersPerSecondSquared (-rawAngular * 2)
    in
    { linear = alignSteering.angular |> Maybe.map accelerationForRotation
    , angular = alignSteering.angular
    }


align :
    { currentRotation : AngularSpeed
    , currentOrientation : Angle
    , targetOrientation : Angle
    }
    -> Steering
align { currentRotation, currentOrientation, targetOrientation } =
    let
        targetRadius =
            Angle.radians 0.017

        slowRadius =
            Angle.radians 0.3

        -- The time over which to achieve target speed
        timeToTarget =
            Duration.seconds 0.1

        rotation =
            targetOrientation
                |> Quantity.minus currentOrientation
                |> Angle.normalize

        rotationSize =
            Quantity.abs rotation
    in
    if rotationSize |> Quantity.lessThan targetRadius then
        none

    else
        let
            targetRotation : AngularSpeed
            targetRotation =
                if rotationSize |> Quantity.greaterThan slowRadius then
                    maxRotation

                else
                    maxRotation |> Quantity.multiplyBy (Quantity.ratio rotationSize slowRadius)

            targetRotationWithDirection : AngularSpeed
            targetRotationWithDirection =
                targetRotation
                    |> Quantity.times rotation
                    |> Quantity.over_ rotationSize

            result : AngularAcceleration
            result =
                targetRotationWithDirection
                    |> Quantity.minus currentRotation
                    |> Quantity.per timeToTarget
                    |> Quantity.clamp
                        (Quantity.negate maxAngularAcceleration)
                        maxAngularAcceleration
        in
        { linear = Nothing
        , angular = Just result
        }


none : Steering
none =
    { linear = Nothing
    , angular = Nothing
    }


accelerate : Steering
accelerate =
    { linear = Just maxAcceleration
    , angular = Nothing
    }


goSlow : Speed -> Steering
goSlow currentVelocity =
    { linear = Just <| reachTargetVelocity currentVelocity (Quantity.half maxVelocity)
    , angular = Nothing
    }


reactToCollision : Steering
reactToCollision =
    -- Start reversing in opposite direction (of velocity)
    -- Similar to applying force from infront of the car
    { linear = Just (maxDeceleration |> Quantity.multiplyBy 15)
    , angular = Nothing
    }


stop : Speed -> Steering
stop currentVelocity =
    { linear = Just <| reachTargetVelocity currentVelocity Quantity.zero
    , angular = Nothing
    }


stopAtDistance : Length -> Length -> Speed -> Steering
stopAtDistance distanceFromTarget threshold currentVelocity =
    let
        nextAcceleration =
            distanceFromTarget
                |> Quantity.minus threshold
                |> Quantity.max Quantity.zero
                |> accelerateToZeroOverDistance currentVelocity
    in
    { linear = Just nextAcceleration
    , angular = Nothing
    }


stopAtPathEnd : LMPoint2d -> Speed -> Route -> Length -> Steering
stopAtPathEnd position velocity route stopRadius =
    let
        target =
            Route.endPoint route

        nextAcceleration =
            case target of
                Just endPoint ->
                    let
                        distanceToParkingSpot =
                            Point2d.distanceFrom position endPoint
                    in
                    if distanceToParkingSpot |> Quantity.lessThanOrEqualTo stopRadius then
                        Quantity.max
                            maxDeceleration
                            (accelerateToZeroOverDistance
                                velocity
                                distanceToParkingSpot
                            )

                    else
                        reachTargetVelocity velocity (Quantity.half maxVelocity)

                Nothing ->
                    maxDeceleration
    in
    { linear = Just nextAcceleration
    , angular = Nothing
    }



--
-- Utility
--


clampVelocity : Speed -> Speed
clampVelocity nextVelocity =
    nextVelocity |> Quantity.clamp minVelocity maxVelocity


reachTargetVelocity : Speed -> Speed -> Acceleration
reachTargetVelocity currentVelocity targetVelocity =
    let
        acceleration =
            if
                Quantity.difference currentVelocity targetVelocity
                    |> Quantity.abs
                    |> Quantity.lessThan (Speed.metersPerSecond 0.1)
            then
                -- With floating point precision the velocity is rarely exactly zero.
                -- With absolute comparison to target speed and a very low acceleration the
                -- car will hover just above and below zero, appearing to be still
                Acceleration.metersPerSecondSquared 0.1

            else
                maxAcceleration
    in
    if currentVelocity |> Quantity.lessThan targetVelocity then
        acceleration

    else
        Quantity.negate acceleration


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance currentVelocity (Quantity distanceFromTarget) =
    if distanceFromTarget < 0.5 then
        -- Very close to the target; try to reach zero velocity
        reachTargetVelocity currentVelocity Quantity.zero

    else
        let
            (Quantity currentSpeed) =
                currentVelocity

            finalSpeed =
                if currentSpeed < 0.1 then
                    -- (Nearly) stopped or reversing before reaching the target; accelerate to move towards the target
                    Speed.inMetersPerSecond maxVelocity * 0.75

                else
                    -- Already in motion; try to achieve optimal deceleration
                    0

            -- Linear acceleration (or deceleration) for a distance from a starting speed to final speed
            acceleration =
                (finalSpeed * finalSpeed - currentSpeed * currentSpeed) / (2 * distanceFromTarget)
        in
        Quantity acceleration
