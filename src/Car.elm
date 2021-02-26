module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
    , boundingBox
    , break
    , build
    , buildRoute
    , giveWay
    , isAtTheEndOfLocalPath
    , isConfused
    , isStoppedOrWaiting
    , isStopping
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
import Direction2d
import Duration
import Geometry exposing (LMBoundingBox2d, LMPoint2d, LocalPath)
import Point2d
import Quantity
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Speed exposing (Speed)
import Tile exposing (Tile(..))


type alias Car =
    { id : Int
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


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding ]


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
    List.member car.status stoppedOrWaiting


isAtTheEndOfLocalPath : Car -> Bool
isAtTheEndOfLocalPath car =
    List.isEmpty car.localPath


isStopping : Car -> Bool
isStopping car =
    car.acceleration |> Quantity.lessThanOrEqualTo acceleration.breakingSlow


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

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
