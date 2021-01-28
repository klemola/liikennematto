module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
    , buildRoute
    , isAtTheEndOfLocalPath
    , isConfused
    , isStoppedOrWaiting
    , linearLocalPathToTarget
    , markAsConfused
    , move
    , new
    , skipRound
    , statusDescription
    , stopAtIntersection
    , turn
    , waitForTrafficLights
    , withHome
    , yield
    )

import Angle
import Config exposing (tileSize)
import CubicSpline2d
import Direction exposing (Direction(..))
import Direction2d exposing (Direction2d)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Position exposing (Position)
import Quantity
import RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Tile exposing (Tile(..))


type alias Car =
    { position : Position
    , rotation : Float
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    , localPath : LocalPath
    }


type alias LocalPath =
    List (Point2d Pixels ())


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
    { position = ( 0, 0 )
    , rotation = Direction.toRadians Up
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
                Point2d.distanceFrom (positionAsPoint2D car.position) next
                    |> Quantity.lessThan (Pixels.pixels 1)
            then
                { car
                    | position = Point2d.toTuple Pixels.inPixels next
                    , localPath = others
                }

            else
                let
                    ( x, y ) =
                        car.position

                    nextRotation =
                        Position.toAngleRadians car.position (Point2d.toTuple Pixels.inPixels next)

                    ( addX, addY ) =
                        fromPolar ( 1, nextRotation )
                in
                { car
                    | position = ( x + addX, y + addY )
                    , rotation = nextRotation
                    , status = Moving
                }

        _ ->
            car


skipRound : Car -> Car
skipRound car =
    { car | status = SkippingRound }


turn : Float -> Car -> Car
turn target car =
    -- TODO: incremental rotation using cubic bézier curves
    { car
        | rotation = target
        , status = Moving
    }


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
    { car | status = Confused, route = [] }


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
            positionAsPoint2D car.position
                |> Debug.log "🏁 origin"

        target =
            positionAsPoint2D node.label.position
                |> Debug.log "🎯 target"

        angleToTarget =
            Direction2d.from origin target
                |> Maybe.map (Direction2d.angleFrom carDirection)
                |> Maybe.withDefault (Angle.degrees 0)
                |> Angle.inDegrees
                |> Debug.log "📐 angle to target"
    in
    if node.label.kind == LotEntry && car.status == ParkedAtLot then
        leaveLotSpline origin target carDirection

    else if node.label.kind == LotEntry then
        linearLocalPathToTarget origin target

    else if abs angleToTarget < 10 then
        -- TODO: check if car is always fully rotated after the previous local path
        linearLocalPathToTarget origin target

    else if abs angleToTarget |> isWithinRoundingErrorOf 90 then
        uTurnSpline origin target carDirection

    else
        curveSpline origin target carDirection


isWithinRoundingErrorOf : Int -> Float -> Bool
isWithinRoundingErrorOf target value =
    round value == target || floor value == target


linearLocalPathToTarget : Point2d Pixels () -> Point2d Pixels () -> LocalPath
linearLocalPathToTarget origin target =
    [ 0, 0.25, 0.5, 0.75, 1 ]
        |> List.map (\step -> Point2d.interpolateFrom origin target step)


leaveLotSpline : Point2d Pixels () -> Point2d Pixels () -> Direction2d () -> LocalPath
leaveLotSpline origin target direction =
    let
        handleDistance =
            Point2d.distanceFrom origin target
                |> Quantity.half

        targetCp =
            target
                |> Point2d.translateIn direction (Pixels.pixels (tileSize / 2))

        midpoint =
            Point2d.midpoint origin targetCp

        handleCp1 =
            Point2d.midpoint origin midpoint
                |> Point2d.translateIn (Direction2d.rotateClockwise direction) handleDistance

        handleCp2 =
            midpoint
                |> Point2d.translateIn (Direction2d.rotateCounterclockwise direction) handleDistance
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


uTurnSpline : Point2d Pixels () -> Point2d Pixels () -> Direction2d () -> LocalPath
uTurnSpline origin target direction =
    let
        handleCp1 =
            origin
                |> Point2d.translateIn direction (Pixels.pixels (tileSize / 4))

        handleCp2 =
            target
                |> Point2d.translateIn direction (Pixels.pixels (tileSize / 4))
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


curveSpline : Point2d Pixels () -> Point2d Pixels () -> Direction2d () -> LocalPath
curveSpline origin target direction =
    let
        handleCp1 =
            origin
                |> Point2d.translateIn direction (Pixels.pixels (tileSize / 4))

        handleCp2 =
            origin
                |> Point2d.translateIn direction (Pixels.pixels (tileSize / 2))
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


positionAsPoint2D : Position -> Point2d Pixels.Pixels ()
positionAsPoint2D position =
    Point2d.fromTuple Pixels.pixels position


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
