module Simulation.Steering exposing
    ( Steering
    , accelerate
    , accelerateToZeroOverDistance
    , align
    , clampVelocity
    , goSlow
    , maxDeceleration
    , maxVelocity
    , none
    , reachTargetVelocity
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
import Common exposing (GlobalCoordinates, isCloseToZeroVelocity)
import Duration
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity(..))
import Simulation.Route as Route exposing (Route)
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
            -- The time over which to achieve target speed
            timeToTarget =
                Duration.seconds 0.1

            slowRadius =
                Angle.radians 0.3

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


goSlow : Speed -> Maybe Speed -> Steering
goSlow currentVelocity targetVelocity =
    { linear =
        targetVelocity
            |> Maybe.withDefault (Quantity.half maxVelocity)
            |> reachTargetVelocity currentVelocity
            |> Just
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
        stopDistance =
            distanceFromTarget |> Quantity.minus threshold

        nextAcceleration =
            if stopDistance |> Quantity.lessThanOrEqualToZero then
                reachTargetVelocity currentVelocity Quantity.zero

            else
                accelerateToZeroOverDistance currentVelocity stopDistance
    in
    { linear = Just nextAcceleration
    , angular = Nothing
    }


stopAtPathEnd : Point2d Length.Meters GlobalCoordinates -> Speed -> Route -> Length -> Steering
stopAtPathEnd currentPosition velocity route stopRadius =
    case route of
        Route.Unrouted ->
            stop velocity

        _ ->
            let
                distanceToPathEnd =
                    Point2d.distanceFrom
                        (Route.endPoint route |> Maybe.withDefault currentPosition)
                        currentPosition

                nextAcceleration =
                    if distanceToPathEnd |> Quantity.lessThanOrEqualTo stopRadius then
                        case Route.distanceToPathEnd route of
                            Just distanceRemaining ->
                                accelerateToZeroOverDistance
                                    velocity
                                    distanceRemaining

                            Nothing ->
                                maxDeceleration

                    else
                        reachTargetVelocity velocity (Quantity.half maxVelocity)
            in
            { linear = Just nextAcceleration
            , angular = Nothing
            }



--
-- Utility
--


clampVelocity : Speed -> Speed
clampVelocity nextVelocity =
    Quantity.clamp minVelocity maxVelocity nextVelocity


reachTargetVelocity : Speed -> Speed -> Acceleration
reachTargetVelocity currentVelocity targetVelocity =
    let
        acceleration =
            if Quantity.difference currentVelocity targetVelocity |> isCloseToZeroVelocity then
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
    if distanceFromTarget == 0 then
        reachTargetVelocity currentVelocity Quantity.zero

    else
        -- Linear acceleration (or deceleration) for a distance from a starting speed to final speed
        -- Original formula: a = (Vf*Vf - Vi*Vi)/(2 * d)
        -- (where Vf = final speed, Vi = starting speed, d = distance, and the result a is acceleration)
        -- ...but with `abs` on the starting speed to handle negative velocity
        let
            (Quantity startingSpeed) =
                currentVelocity

            finalSpeed =
                if distanceFromTarget > 1 && abs startingSpeed < 1 then
                    -- (Nearly) stopped before reaching the target; accelerate to move towards the target
                    Speed.inMetersPerSecond maxVelocity * 0.5

                else
                    -- Already in motion; try to achieve optimal deceleration
                    0
        in
        ((finalSpeed * finalSpeed - startingSpeed * abs startingSpeed) / (2 * distanceFromTarget))
            |> Quantity
            |> Quantity.clamp maxDeceleration maxAcceleration
