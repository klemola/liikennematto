module Steering exposing
    ( Steering
    , align
    , angleToTarget
    , maxAcceleration
    , maxDeceleration
    , maxVelocity
    , noSteering
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import AngularAcceleration exposing (AngularAcceleration)
import AngularSpeed exposing (AngularSpeed)
import Direction2d
import Duration
import Geometry exposing (LMPoint2d)
import Quantity
import Speed exposing (Speed)


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


type alias Steering =
    { linear : Maybe Acceleration
    , angular : Maybe AngularAcceleration
    }


noSteering : Steering
noSteering =
    { linear = Nothing
    , angular = Nothing
    }


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
            Acceleration.metersPerSecondSquared (0 - rawAngular * 2)
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
        noSteering

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



--
-- Utility
--


angleToTarget : LMPoint2d -> LMPoint2d -> Maybe Angle
angleToTarget origin target =
    Direction2d.from origin target
        |> Maybe.map Direction2d.toAngle
