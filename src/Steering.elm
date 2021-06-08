module Steering exposing
    ( Steering
    , align
    , followPath
    , maxAcceleration
    , maxDeceleration
    , maxVelocity
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import AngularAcceleration exposing (AngularAcceleration)
import AngularSpeed exposing (AngularSpeed)
import Direction2d
import Duration
import Geometry exposing (LMPoint2d)
import LocalPath exposing (LocalPath)
import Quantity
import Speed exposing (Speed)


maxVelocity : Speed
maxVelocity =
    -- TODO change this back to 11.1
    Speed.metersPerSecond 3


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
    { linear = Nothing, angular = Nothing }


followPath :
    { currentRotation : AngularSpeed
    , currentOrientation : Angle
    , currentPosition : LMPoint2d
    , path : LocalPath
    }
    -> Steering
followPath { currentRotation, currentOrientation, currentPosition, path } =
    let
        targetOrientation =
            List.head path
                |> Maybe.andThen (angleToTarget currentPosition)
    in
    case targetOrientation of
        Just orientation ->
            align
                { currentRotation = currentRotation
                , currentOrientation = currentOrientation
                , targetOrientation = orientation
                }

        Nothing ->
            noSteering


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
                    maxRotation
                        |> Quantity.times rotationSize
                        |> Quantity.over_ slowRadius

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

            angularAcceleration =
                Quantity.abs result

            resultWithMaxLimit =
                if angularAcceleration |> Quantity.greaterThan maxAngularAcceleration then
                    result
                        |> Quantity.times maxAngularAcceleration
                        |> Quantity.over angularAcceleration

                else
                    result
        in
        { linear = Nothing
        , angular = Just resultWithMaxLimit
        }



--
-- Utility
--


angleToTarget : LMPoint2d -> LMPoint2d -> Maybe Angle
angleToTarget origin target =
    Direction2d.from origin target
        |> Maybe.map Direction2d.toAngle
