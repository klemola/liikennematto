module Car exposing
    ( Car
    , CarKind(..)
    , Cars
    , Status(..)
    , boundingBox
    , break
    , build
    , createRoute
    , giveWay
    , isAtTheEndOfLocalPath
    , isBreaking
    , isConfused
    , isStoppedOrWaiting
    , markAsConfused
    , move
    , new
    , startMoving
    , statusDescription
    , stopAtIntersection
    , waitForTrafficLights
    , withHome
    , withPosition
    , withRotation
    , withVelocity
    , yield
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Config exposing (acceleration, carWidth, maxVelocity)
import Dict exposing (Dict)
import Direction2d
import Duration
import Entity exposing (Id)
import Geometry
    exposing
        ( LMBoundingBox2d
        , LMEntityDistance
        , LMPoint2d
        , LocalPath
        , trafficLightsStopMargin
        )
import Length exposing (Length)
import Point2d
import Quantity exposing (Quantity(..))
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)


type alias Car =
    { id : Id
    , position : LMPoint2d
    , rotation : Angle
    , velocity : Speed
    , acceleration : Acceleration
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    , localPath : LocalPath
    }


type alias NewCar =
    { position : LMPoint2d
    , rotation : Angle
    , velocity : Speed
    , acceleration : Acceleration
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    , localPath : LocalPath
    }


type alias Cars =
    Dict Id Car


type CarKind
    = SedanA
    | SedanB
    | SedanC
    | SedanD
    | SedanE
    | TestCar


type Status
    = Moving
    | WaitingForTrafficLights
    | StoppedAtIntersection
    | Yielding
    | ParkedAtLot
    | Confused


new : CarKind -> NewCar
new kind =
    { position = Point2d.origin
    , rotation = Angle.degrees 0
    , velocity = Quantity.zero
    , acceleration = acceleration.speedUp
    , kind = kind
    , status = Confused
    , homeLotId = Nothing
    , route = []
    , localPath = []
    }


withHome : Int -> NewCar -> NewCar
withHome lotId car =
    { car | homeLotId = Just lotId }


withPosition : LMPoint2d -> NewCar -> NewCar
withPosition position car =
    { car | position = position }


withRotation : Angle -> NewCar -> NewCar
withRotation rotation car =
    { car | rotation = rotation }


withVelocity : Speed -> NewCar -> NewCar
withVelocity velocity car =
    { car | velocity = velocity }


build : Int -> NewCar -> Car
build id newCar =
    { id = id
    , position = newCar.position
    , rotation = newCar.rotation
    , velocity = newCar.velocity
    , acceleration = newCar.acceleration
    , kind = newCar.kind
    , status = newCar.status
    , homeLotId = newCar.homeLotId
    , route = newCar.route
    , localPath = newCar.localPath
    }


isConfused : Car -> Bool
isConfused car =
    car.status == Confused


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    car.velocity == Quantity.zero


isAtTheEndOfLocalPath : Car -> Bool
isAtTheEndOfLocalPath car =
    List.isEmpty car.localPath


isBreaking : Car -> Bool
isBreaking car =
    car.acceleration |> Quantity.lessThan Quantity.zero


boundingBox : Car -> LMBoundingBox2d
boundingBox car =
    -- Room for improvement: use a more realistic bounding box
    Geometry.boundingBoxFromCircle car.position (carWidth / 1.5)


move : Car -> Car
move car =
    case car.localPath of
        next :: others ->
            if
                car.position
                    |> Geometry.isPointAt next
            then
                { car
                    | position = next
                    , localPath = others
                }

            else
                let
                    carDirection =
                        Direction2d.fromAngle car.rotation

                    nextVelocity =
                        car.velocity
                            |> Quantity.plus (Quantity.for (Duration.milliseconds 16) car.acceleration)
                            |> Quantity.clamp Quantity.zero maxVelocity

                    nextPosition =
                        car.position
                            |> Point2d.at_ Geometry.pixelsToMetersRatio
                            |> Point2d.translateIn carDirection (Quantity.for (Duration.milliseconds 16) nextVelocity)
                            |> Point2d.at Geometry.pixelsToMetersRatio
                in
                { car
                    | position = nextPosition
                    , velocity = nextVelocity
                    , rotation =
                        car.position
                            |> Geometry.angleToTarget next
                }

        _ ->
            car


waitForTrafficLights : LMEntityDistance -> Car -> Car
waitForTrafficLights distanceFromTrafficLight car =
    let
        targetDistance =
            distanceFromTrafficLight
                |> Quantity.minus trafficLightsStopMargin
                |> Quantity.max Quantity.zero
                |> Geometry.pixelsToLength
    in
    { car
        | status = WaitingForTrafficLights
        , acceleration = accelerateToZeroOverDistance car.velocity targetDistance
    }


yield : Car -> Car
yield car =
    { car
        | status = Yielding
        , acceleration = Quantity.zero
        , velocity = Quantity.zero
    }


stopAtIntersection : Car -> Car
stopAtIntersection car =
    { car
        | status = StoppedAtIntersection
        , acceleration = Quantity.zero
        , velocity = Quantity.zero
    }


giveWay : Car -> Car
giveWay car =
    { car | acceleration = acceleration.breakingSlow }


break : Car -> Car
break car =
    { car | acceleration = acceleration.breakingFast }


startMoving : Car -> Car
startMoving car =
    { car
        | status = Moving
        , acceleration = acceleration.speedUp
    }


markAsConfused : Car -> Car
markAsConfused car =
    { car
        | status = Confused
        , velocity = Quantity.zero
        , acceleration = Quantity.zero
        , route = []
        , localPath = []
    }


createRoute : RNNodeContext -> Car -> Car
createRoute nodeCtx car =
    { car
        | route = [ nodeCtx ]
        , localPath = localPathToTarget car nodeCtx
    }


localPathToTarget : Car -> RNNodeContext -> LocalPath
localPathToTarget car { node } =
    let
        carDirection =
            Direction2d.fromAngle car.rotation

        origin =
            car.position

        target =
            node.label.position

        angleDegreesToTarget =
            origin
                |> Geometry.angleFromDirection carDirection target
                |> Angle.inDegrees
    in
    if node.label.kind == LotEntry && car.status == ParkedAtLot then
        Geometry.leaveLotSpline origin target carDirection

    else if node.label.kind == LotEntry then
        Geometry.linearLocalPathToTarget origin target

    else if abs angleDegreesToTarget < 10 then
        Geometry.linearLocalPathToTarget origin target

    else if abs angleDegreesToTarget |> isWithinRoundingErrorOf 90 then
        Geometry.uTurnSpline origin target carDirection

    else
        Geometry.curveSpline origin target carDirection


isWithinRoundingErrorOf : Int -> Float -> Bool
isWithinRoundingErrorOf target value =
    round value == target || floor value == target


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance (Quantity speed) (Quantity distanceToTarget) =
    Quantity (-speed * speed / (2 * distanceToTarget))



--
-- Debug helpers
--


statusDescription : Car -> String
statusDescription car =
    case car.status of
        Moving ->
            "Moving" ++ " " ++ routeDescription car.route

        WaitingForTrafficLights ->
            "Waiting for traffic lights"

        StoppedAtIntersection ->
            "Stopped"

        Yielding ->
            "Yielding"

        ParkedAtLot ->
            "Parked @ lot"

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
