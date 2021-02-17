module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
    , boundingBox
    , break
    , buildRoute
    , giveWay
    , isAtTheEndOfLocalPath
    , isConfused
    , isStoppedOrWaiting
    , markAsConfused
    , maxVelocity
    , move
    , new
    , startMoving
    , statusDescription
    , stopAtIntersection
    , waitForTrafficLights
    , withDefaultVelocityAndAcceleration
    , withHome
    , withPosition
    , withRotation
    , withRoute
    , withVelocity
    , yield
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import Config exposing (carWidth)
import Direction2d
import Duration
import Geometry exposing (LMBoundingBox2d, LMPoint2d, LocalPath)
import Point2d
import Quantity
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)
import Tile exposing (Tile(..))


type alias Car =
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
    | Stopping
    | Confused


maxVelocity =
    Speed.metersPerSecond 11.1


speedUp =
    Acceleration.metersPerSecondSquared 5


breakingSlow =
    Acceleration.metersPerSecondSquared -15


breakingFast =
    Acceleration.metersPerSecondSquared -30


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding ]


new : CarKind -> Car
new kind =
    { position = Point2d.origin
    , rotation = Angle.degrees 0
    , velocity = Quantity.zero
    , acceleration = speedUp
    , kind = kind
    , status = Confused
    , homeLotId = Nothing
    , route = []
    , localPath = []
    }


withHome : Int -> Car -> Car
withHome lotId car =
    { car | homeLotId = Just lotId }


withPosition : LMPoint2d -> Car -> Car
withPosition position car =
    { car | position = position }


withRotation : Angle -> Car -> Car
withRotation rotation car =
    { car | rotation = rotation }


withVelocity : Speed -> Car -> Car
withVelocity velocity car =
    { car | velocity = velocity }


withAcceleration : Acceleration -> Car -> Car
withAcceleration acceleration car =
    { car | acceleration = acceleration }


withDefaultVelocityAndAcceleration : Car -> Car
withDefaultVelocityAndAcceleration car =
    car
        |> withVelocity Quantity.zero
        |> withAcceleration speedUp


withRoute : RNNodeContext -> Car -> Car
withRoute nodeCtx car =
    buildRoute car nodeCtx


isConfused : Car -> Bool
isConfused car =
    car.status == Confused


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    List.member car.status stoppedOrWaiting


isAtTheEndOfLocalPath : Car -> Bool
isAtTheEndOfLocalPath car =
    List.isEmpty car.localPath


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

                    nextStatus =
                        if car.acceleration |> Quantity.lessThan speedUp then
                            Stopping

                        else
                            car.status
                in
                { car
                    | position = nextPosition
                    , velocity = nextVelocity
                    , rotation =
                        car.position
                            |> Geometry.angleToTarget next
                    , status = nextStatus
                }

        _ ->
            car


waitForTrafficLights : Car -> Car
waitForTrafficLights car =
    { car | status = WaitingForTrafficLights }


yield : Car -> Car
yield car =
    { car | status = Yielding }


stopAtIntersection : Car -> Car
stopAtIntersection car =
    { car | status = StoppedAtIntersection }


giveWay : Car -> Car
giveWay car =
    { car
        | status = Moving
        , acceleration = breakingSlow
    }


break : Car -> Car
break car =
    { car
        | status = Stopping
        , acceleration = breakingFast
    }


startMoving : Car -> Car
startMoving car =
    { car
        | status = Moving
    }
        |> withDefaultVelocityAndAcceleration


markAsConfused : Car -> Car
markAsConfused car =
    { car
        | status = Confused
        , route = []
        , localPath = []
    }


beginLeaveLot : Car -> Car
beginLeaveLot car =
    case car.route of
        target :: _ ->
            buildRoute car target

        _ ->
            car


buildRoute : Car -> RNNodeContext -> Car
buildRoute car nodeCtx =
    { car
        | route = [ nodeCtx ]
        , localPath = localPathToTarget car nodeCtx
        , status = Moving
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


statusDescription : Car -> String
statusDescription car =
    case car.status of
        Moving ->
            "Moving" ++ " " ++ routeDescription car.route

        WaitingForTrafficLights ->
            "Stopped @ traffic lights"

        StoppedAtIntersection ->
            "Stopped"

        Yielding ->
            "Yielding"

        ParkedAtLot ->
            "Parked @ lot"

        Stopping ->
            "Stopping"

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
