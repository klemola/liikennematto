module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
    , boundingBox
    , buildRoute
    , giveWay
    , isAtTheEndOfLocalPath
    , isConfused
    , isStoppedOrWaiting
    , markAsConfused
    , move
    , new
    , statusDescription
    , stopAtIntersection
    , waitForTrafficLights
    , withHome
    , withPosition
    , withRotation
    , withRoute
    , yield
    )

import Angle exposing (Angle)
import Config exposing (carSize)
import Direction2d
import Geometry exposing (LMBoundingBox2d, LMPoint2d, LocalPath)
import Point2d
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Tile exposing (Tile(..))


type alias Car =
    { position : LMPoint2d
    , rotation : Angle
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
    | GivingWay
    | Confused


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding ]


new : CarKind -> Car
new kind =
    { position = Point2d.origin
    , rotation = Angle.degrees 0
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
    Geometry.boundingBoxFromCircle car.position carSize


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
                    , status = Moving
                }

            else
                let
                    carDirection =
                        Direction2d.fromAngle car.rotation

                    nextPosition =
                        car.position
                            |> Geometry.translatePointIn carDirection 1
                in
                { car
                    | position = nextPosition
                    , rotation =
                        car.position
                            |> Geometry.angleToTarget next
                    , status = Moving
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
    { car | status = GivingWay }


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

        GivingWay ->
            "Giving way"

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
