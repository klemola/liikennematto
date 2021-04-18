module Car exposing
    ( Car
    , CarKind(..)
    , Cars
    , Status(..)
    , boundingBox
    , break
    , build
    , createRoute
    , defaultAcceleration
    , fieldOfView
    , isAtTheEndOfLocalPath
    , isBreaking
    , isConfused
    , isStoppedOrWaiting
    , length
    , markAsConfused
    , maxDeceleration
    , maxVelocity
    , move
    , new
    , rightSideOfFieldOfView
    , startMoving
    , statusDescription
    , stopAtIntersection
    , waitForTrafficLights
    , width
    , withHome
    , withPosition
    , withRotation
    , withVelocity
    , yield
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Config
    exposing
        ( pixelsToMeters
        )
import Dict exposing (Dict)
import Direction2d
import Duration
import Entity exposing (Id)
import Geometry
    exposing
        ( LMBoundingBox2d
        , LMEntityCoordinates
        , LMPoint2d
        )
import Length exposing (Length, Meters)
import LocalPath exposing (LocalPath)
import Point2d
import Quantity exposing (Quantity(..))
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


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


maxVelocity : Speed
maxVelocity =
    Speed.metersPerSecond 11.1


length : Length
length =
    pixelsToMeters 24


width : Length
width =
    pixelsToMeters 12


fieldOfView : Angle
fieldOfView =
    Angle.degrees 70


trafficLightsStopMargin : Length
trafficLightsStopMargin =
    Length.meters 5


collisionMargin : Length
collisionMargin =
    length |> Quantity.multiplyBy 1.5


defaultAcceleration : Acceleration
defaultAcceleration =
    Acceleration.metersPerSecondSquared 5


maxDeceleration : Acceleration
maxDeceleration =
    Acceleration.metersPerSecondSquared -20


collisionCircleRadius : Length
collisionCircleRadius =
    width |> Quantity.divideBy 1.5


new : CarKind -> NewCar
new kind =
    { position = Point2d.origin
    , rotation = Angle.degrees 0
    , velocity = Quantity.zero
    , acceleration = defaultAcceleration
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
    Geometry.boundingBoxFromCircle car.position collisionCircleRadius


move : Car -> Car
move car =
    case car.localPath of
        next :: others ->
            if Point2d.equalWithin (Length.meters 0.1) car.position next then
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
                            |> Quantity.plus (car.acceleration |> Quantity.for (Duration.milliseconds 16))
                            |> Quantity.clamp Quantity.zero maxVelocity

                    nextPosition =
                        car.position
                            |> Point2d.translateIn carDirection (nextVelocity |> Quantity.for (Duration.milliseconds 16))
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


waitForTrafficLights : Length -> Car -> Car
waitForTrafficLights distanceFromTrafficLight car =
    let
        targetDistance =
            distanceFromTrafficLight
                |> Quantity.minus trafficLightsStopMargin
                |> Quantity.max Quantity.zero
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


break : Length -> Car -> Car
break distanceToCollision car =
    let
        targetDistance =
            distanceToCollision
                |> Quantity.minus collisionMargin
                |> Quantity.max Quantity.zero

        nextAcceleration =
            Quantity.max maxDeceleration (accelerateToZeroOverDistance car.velocity targetDistance)
    in
    { car | acceleration = nextAcceleration }


startMoving : Car -> Car
startMoving car =
    { car
        | status = Moving
        , acceleration = defaultAcceleration
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
        LocalPath.leaveLotSpline origin target carDirection

    else if node.label.kind == LotEntry then
        LocalPath.linearLocalPathToTarget origin target

    else if abs angleDegreesToTarget < 10 then
        LocalPath.linearLocalPathToTarget origin target

    else if abs angleDegreesToTarget |> isWithinRoundingErrorOf 90 then
        LocalPath.uTurnSpline origin target carDirection

    else
        LocalPath.curveSpline origin target carDirection


isWithinRoundingErrorOf : Int -> Float -> Bool
isWithinRoundingErrorOf target value =
    round value == target || floor value == target


accelerateToZeroOverDistance : Speed -> Length -> Acceleration
accelerateToZeroOverDistance (Quantity speed) (Quantity distanceToTarget) =
    Quantity (-speed * speed / (2 * distanceToTarget))


rightSideOfFieldOfView : Length -> Car -> Triangle2d Meters LMEntityCoordinates
rightSideOfFieldOfView distance car =
    -- Room for improvement: for realism, the field of view should change by velocity
    let
        direction =
            Direction2d.fromAngle car.rotation

        limitFront =
            car.position |> Point2d.translateIn direction distance

        angle =
            Quantity.half fieldOfView
                |> Quantity.negate

        distanceRight =
            distance |> Quantity.divideBy (Angle.cos angle)

        limitRight =
            car.position
                |> Point2d.translateIn
                    (Direction2d.rotateBy angle direction)
                    distanceRight
    in
    Triangle2d.fromVertices ( car.position, limitFront, limitRight )



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
