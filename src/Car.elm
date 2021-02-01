module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
    , buildRoute
    , isAtTheEndOfLocalPath
    , isConfused
    , isStoppedOrWaiting
    , markAsConfused
    , move
    , new
    , skipRound
    , statusDescription
    , stopAtIntersection
    , waitForTrafficLights
    , withHome
    , yield
    )

import Angle
import Config exposing (tileSize)
import CubicSpline2d
import Direction2d exposing (Direction2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Quantity
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Tile exposing (Tile(..))


type alias LMPoint2d =
    Point2d Pixels ()


type alias Car =
    { position : LMPoint2d
    , rotation : Float
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    , localPath : LocalPath
    }


type alias LocalPath =
    List LMPoint2d


type CarKind
    = SedanA
    | SedanB
    | SedanC
    | SedanD
    | SedanE


type Status
    = Moving
    | WaitingForTrafficLights
    | StoppedAtIntersection
    | Yielding
    | ParkedAtLot
    | SkippingRound
    | Confused


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding, SkippingRound ]


new : CarKind -> Car
new kind =
    { position = Point2d.origin

    -- TODO: check
    , rotation = 0
    , kind = kind
    , status = Confused
    , homeLotId = Nothing
    , route = []
    , localPath = []
    }


withHome : Int -> Car -> Car
withHome lotId car =
    { car | homeLotId = Just lotId }


isConfused : Car -> Bool
isConfused car =
    car.status == Confused


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    List.member car.status stoppedOrWaiting


isAtTheEndOfLocalPath : Car -> Bool
isAtTheEndOfLocalPath car =
    List.isEmpty car.localPath


move : Car -> Car
move car =
    case car.localPath of
        next :: others ->
            if
                Point2d.distanceFrom car.position next
                    |> Quantity.lessThan (Pixels.pixels 1)
            then
                { car
                    | position = next
                    , localPath = others
                }

            else
                let
                    carDirection =
                        Direction2d.radians car.rotation

                    angleToTarget =
                        Direction2d.from car.position next
                            |> Maybe.map Direction2d.toAngle
                            |> Maybe.withDefault (Angle.degrees 0)

                    nextPosition =
                        Point2d.translateIn carDirection (Pixels.pixels 1) car.position
                in
                { car
                    | position = nextPosition
                    , rotation =
                        angleToTarget
                            |> Angle.inRadians
                    , status = Moving
                }

        _ ->
            car


skipRound : Car -> Car
skipRound car =
    { car | status = SkippingRound }


waitForTrafficLights : Car -> Car
waitForTrafficLights car =
    { car | status = WaitingForTrafficLights }


yield : Car -> Car
yield car =
    { car | status = Yielding }


stopAtIntersection : Car -> Car
stopAtIntersection car =
    { car | status = StoppedAtIntersection }


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
            Direction2d.radians car.rotation

        origin =
            car.position

        target =
            node.label.position

        angleToTarget =
            Direction2d.from origin target
                |> Maybe.map (Direction2d.angleFrom carDirection)
                |> Maybe.withDefault (Angle.degrees 0)
                |> Angle.inDegrees
    in
    if node.label.kind == LotEntry && car.status == ParkedAtLot then
        leaveLotSpline origin target carDirection

    else if node.label.kind == LotEntry then
        linearLocalPathToTarget origin target

    else if abs angleToTarget < 10 then
        linearLocalPathToTarget origin target

    else if abs angleToTarget |> isWithinRoundingErrorOf 90 then
        uTurnSpline origin target carDirection

    else
        curveSpline origin target carDirection


isWithinRoundingErrorOf : Int -> Float -> Bool
isWithinRoundingErrorOf target value =
    round value == target || floor value == target


linearLocalPathToTarget : LMPoint2d -> LMPoint2d -> LocalPath
linearLocalPathToTarget origin target =
    [ 0, 0.25, 0.5, 0.75, 1 ]
        |> List.map (\step -> Point2d.interpolateFrom origin target step)


leaveLotSpline : LMPoint2d -> LMPoint2d -> Direction2d () -> LocalPath
leaveLotSpline origin target direction =
    let
        handleDistance =
            Point2d.distanceFrom origin target
                |> Quantity.half

        targetCp =
            Point2d.translateIn direction (Pixels.pixels (tileSize / 2)) target

        midpoint =
            Point2d.midpoint origin targetCp

        handleCp1 =
            midpoint
                |> Point2d.midpoint origin
                |> Point2d.translateIn (Direction2d.rotateClockwise direction) handleDistance

        handleCp2 =
            Point2d.translateIn (Direction2d.rotateCounterclockwise direction) handleDistance midpoint
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


uTurnSpline : LMPoint2d -> LMPoint2d -> Direction2d () -> LocalPath
uTurnSpline origin target direction =
    let
        turnDistance =
            Pixels.pixels (tileSize / 4)

        handleCp1 =
            Point2d.translateIn direction turnDistance origin

        handleCp2 =
            Point2d.translateIn direction turnDistance target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


curveSpline : LMPoint2d -> LMPoint2d -> Direction2d () -> LocalPath
curveSpline origin target direction =
    let
        angleToTarget =
            Direction2d.from origin target
                |> Maybe.map (Direction2d.angleFrom direction)
                |> Maybe.withDefault (Angle.degrees 0)

        distanceToTarget =
            Point2d.distanceFrom origin target

        distanceToCorner =
            Quantity.multiplyBy (Angle.cos angleToTarget) distanceToTarget

        corner =
            Point2d.translateIn direction distanceToCorner origin

        handleCp1 =
            Point2d.midpoint origin corner

        handleCp2 =
            Point2d.midpoint corner target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


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

        SkippingRound ->
            "Skipping the round"

        Confused ->
            "Confused"


routeDescription : List RNNodeContext -> String
routeDescription route =
    case route of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
