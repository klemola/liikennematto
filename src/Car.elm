module Car exposing
    ( Car
    , CarKind(..)
    , Cars
    , Status(..)
    , break
    , build
    , createRoute
    , defaultAcceleration
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
    , secondsTo
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
import BoundingBox2d
import Dict exposing (Dict)
import Direction2d
import Duration
import Entity exposing (Id)
import Frame2d
import Geometry
    exposing
        ( LMBoundingBox2d
        , LMEntityCoordinates
        , LMPoint2d
        )
import Length exposing (Length, Meters)
import LocalPath exposing (LocalPath)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity exposing (Quantity(..), Rate)
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


type alias Car =
    { id : Id
    , position : LMPoint2d
    , rotation : Angle
    , velocity : Speed
    , acceleration : Acceleration
    , shape : Polygon2d Meters LMEntityCoordinates
    , boundingBox : LMBoundingBox2d
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



--
-- Constants
--


maxVelocity : Speed
maxVelocity =
    Speed.metersPerSecond 11.1


length : Length
length =
    Length.meters 4.6


width : Length
width =
    Length.meters 2.3


shape : Polygon2d Meters LMEntityCoordinates
shape =
    -- the shape of a car with centroid of origin
    let
        halfLength =
            Quantity.half length

        halfWidth =
            Quantity.half width

        p1 =
            Point2d.xy
                (Quantity.negate halfLength)
                (Quantity.negate halfWidth)

        p2 =
            Point2d.xy
                halfLength
                (Quantity.negate halfWidth)

        p3 =
            Point2d.xy
                halfLength
                halfWidth

        p4 =
            Point2d.xy
                (Quantity.negate halfLength)
                halfWidth
    in
    Polygon2d.singleLoop [ p1, p2, p3, p4 ]


maxFieldOfView : Angle
maxFieldOfView =
    Angle.degrees 100


speedToFieldOfViewReduction : Quantity Float (Rate Speed.MetersPerSecond Angle.Radians)
speedToFieldOfViewReduction =
    Speed.metersPerSecond 2.5 |> Quantity.per (Angle.degrees 10)


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



--
-- Builder
--


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
    let
        nextShape =
            adjustedShape newCar.position newCar.rotation

        boundingBox =
            Polygon2d.boundingBox nextShape
                |> Maybe.withDefault (BoundingBox2d.singleton newCar.position)
    in
    { id = id
    , position = newCar.position
    , rotation = newCar.rotation
    , velocity = newCar.velocity
    , acceleration = newCar.acceleration
    , shape = nextShape
    , boundingBox = boundingBox
    , kind = newCar.kind
    , status = newCar.status
    , homeLotId = newCar.homeLotId
    , route = newCar.route
    , localPath = newCar.localPath
    }



--
-- Queries
--


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


secondsTo : LMPoint2d -> Car -> Quantity Float Duration.Seconds
secondsTo target car =
    let
        distanceToTarget =
            Point2d.distanceFrom car.position target
    in
    distanceToTarget |> Quantity.at_ car.velocity



--
-- Modification
--


move : Car -> Car
move car =
    case car.localPath of
        next :: others ->
            let
                atLocalPathPoint =
                    Point2d.equalWithin (Length.meters 0.1) car.position next

                carDirection =
                    Direction2d.fromAngle car.rotation

                nextLocalPath =
                    if atLocalPathPoint then
                        others

                    else
                        car.localPath

                nextPosition =
                    if atLocalPathPoint then
                        next

                    else
                        car.position
                            |> Point2d.translateIn carDirection (nextVelocity |> Quantity.for (Duration.milliseconds 16))

                nextVelocity =
                    car.velocity
                        |> Quantity.plus (car.acceleration |> Quantity.for (Duration.milliseconds 16))
                        |> Quantity.clamp Quantity.zero maxVelocity

                nextRotation =
                    if atLocalPathPoint then
                        car.rotation

                    else
                        car.position |> Geometry.angleToTarget next

                nextShape =
                    adjustedShape nextPosition nextRotation

                nextBoundingBox =
                    Polygon2d.boundingBox shape
                        |> Maybe.withDefault (BoundingBox2d.singleton nextPosition)
            in
            { car
                | position = nextPosition
                , velocity = nextVelocity
                , rotation = nextRotation
                , shape = nextShape
                , boundingBox = nextBoundingBox
                , localPath = nextLocalPath
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
        , acceleration =
            if targetDistance |> Quantity.lessThanOrEqualTo trafficLightsStopMargin then
                maxDeceleration

            else
                accelerateToZeroOverDistance car.velocity targetDistance
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



--
-- Logic helpers
--


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
    let
        direction =
            Direction2d.fromAngle car.rotation

        limitFront =
            car.position |> Point2d.translateIn direction distance

        fieldOfViewReduction =
            car.velocity |> Quantity.at_ speedToFieldOfViewReduction

        fieldOfView =
            maxFieldOfView |> Quantity.minus fieldOfViewReduction

        angle =
            Quantity.half fieldOfView |> Quantity.negate

        distanceRight =
            distance |> Quantity.divideBy (Angle.cos angle)

        limitRight =
            car.position
                |> Point2d.translateIn
                    (Direction2d.rotateBy angle direction)
                    distanceRight
    in
    Triangle2d.from car.position limitFront limitRight


adjustedShape : LMPoint2d -> Angle -> Polygon2d Meters LMEntityCoordinates
adjustedShape nextPosition nextRotation =
    let
        carFrame =
            Frame2d.atPoint nextPosition |> Frame2d.rotateBy nextRotation
    in
    shape |> Polygon2d.placeIn carFrame



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
