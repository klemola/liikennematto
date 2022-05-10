module Simulation.Steering exposing
    ( Steering
    , align
    , applyCollisionEffects
    , break
    , maxAcceleration
    , maxDeceleration
    , maxVelocity
    , noSteering
    , seekAndFaceTarget
    , slowDown
    , startMoving
    , stop
    , stopAtPathEnd
    , stopAtTrafficControl
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import AngularAcceleration exposing (AngularAcceleration)
import AngularSpeed exposing (AngularSpeed)
import Duration
import Length exposing (Length)
import Model.Car exposing (Car)
import Point2d
import Polyline2d
import Quantity exposing (Quantity(..))
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


trafficControlStopDistance : Length
trafficControlStopDistance =
    Length.meters 4


parkingRadius : Length
parkingRadius =
    Length.meters 10


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


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance (Quantity currentSpeed) (Quantity distanceFromTarget) =
    if distanceFromTarget < 0.5 then
        maxDeceleration

    else
        let
            finalSpeed =
                if currentSpeed < 0.1 then
                    -- (Nearly) stopped before reaching the target; accelerate to move towards the target
                    Speed.inMetersPerSecond maxVelocity * 0.75

                else
                    -- Already in motion; try to achieve optimal deceleration
                    0

            -- Linear acceleration (or deceleration) for a distance from a starting speed to final speed
            acceleration =
                (finalSpeed * finalSpeed - currentSpeed * currentSpeed) / (2 * distanceFromTarget)
        in
        Quantity acceleration



--
-- Imported from Car.elm
--


stopAtTrafficControl : Length -> Car -> Car
stopAtTrafficControl distanceFromTrafficControl car =
    let
        nextAcceleration =
            if car.velocity == Quantity.zero then
                maxDeceleration

            else
                distanceFromTrafficControl
                    |> Quantity.minus trafficControlStopDistance
                    |> Quantity.max Quantity.zero
                    |> accelerateToZeroOverDistance car.velocity
                    |> Quantity.clamp maxDeceleration Quantity.zero
    in
    { car | acceleration = nextAcceleration }


startMoving : Car -> Car
startMoving car =
    { car | acceleration = maxAcceleration }


break : Length -> Car -> Car
break breakDistance car =
    let
        collisionMargin =
            car.make.length |> Quantity.multiplyBy 1.5

        targetDistance =
            breakDistance
                |> Quantity.minus collisionMargin
                |> Quantity.max Quantity.zero

        nextAcceleration =
            Quantity.max maxDeceleration (accelerateToZeroOverDistance car.velocity targetDistance)
    in
    { car | acceleration = nextAcceleration }


slowDown : Speed -> Car -> Car
slowDown targetVelocity car =
    { car | acceleration = reachTargetVelocity car.velocity targetVelocity }


stop : Car -> Car
stop car =
    { car | acceleration = maxDeceleration }


stopAtPathEnd : Car -> Car
stopAtPathEnd car =
    let
        target =
            -- Room for improvement: this is slow. Try to cache the endpoint of the path or just use the spline instead
            car.localPath
                |> Polyline2d.vertices
                |> List.reverse
                |> List.head

        nextAcceleration =
            case target of
                Just endPoint ->
                    let
                        distanceToParkingSpot =
                            Point2d.distanceFrom car.position endPoint
                    in
                    if distanceToParkingSpot |> Quantity.lessThanOrEqualTo parkingRadius then
                        Quantity.max
                            maxDeceleration
                            (accelerateToZeroOverDistance
                                car.velocity
                                distanceToParkingSpot
                            )

                    else
                        reachTargetVelocity car.velocity (Quantity.half maxVelocity)

                Nothing ->
                    maxDeceleration
    in
    { car | acceleration = nextAcceleration }


applyCollisionEffects : Car -> Car
applyCollisionEffects car =
    -- Bounce the car back on impact
    { car
        | acceleration = Quantity.zero
        , velocity = Speed.metersPerSecond -10
    }


reachTargetVelocity : Speed -> Speed -> Acceleration
reachTargetVelocity currentVelocity targetVelocity =
    if currentVelocity |> Quantity.greaterThan targetVelocity then
        Acceleration.metersPerSecondSquared -5

    else
        maxAcceleration
