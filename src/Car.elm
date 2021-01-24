module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , beginLeaveLot
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

import Config exposing (tileSize)
import CubicSpline2d
import Direction exposing (Direction(..))
import Direction2d
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Polyline2d
import Position exposing (Position)
import Quantity
import RoadNetwork exposing (RNNodeContext)
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


positionAsPoint2D : Car -> Point2d Pixels.Pixels ()
positionAsPoint2D car =
    Point2d.fromTuple Pixels.pixels car.position


move : Car -> Car
move car =
    case car.localPath of
        next :: others ->
            if
                Point2d.distanceFrom (positionAsPoint2D car) next
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
            -- TODO: check if the node is a LotEntry
            { car
                | status = Moving
                , localPath = leaveLotSpline car.position target.node.label.position car.rotation
            }

        _ ->
            car


linearLocalPathToTarget : Position -> Car -> LocalPath
linearLocalPathToTarget target car =
    let
        start =
            positionAsPoint2D car

        end =
            Point2d.fromTuple Pixels.pixels target
    in
    List.map (\step -> Point2d.interpolateFrom start end step) [ 0, 0.25, 0.5, 0.75, 1 ]


leaveLotSpline : Position -> Position -> Float -> LocalPath
leaveLotSpline ( originX, originY ) ( targetX, targetY ) rotation =
    let
        directionToTarget =
            Direction2d.radians rotation

        origin =
            Point2d.pixels originX originY

        target =
            Point2d.pixels targetX targetY

        handleDistance =
            Point2d.distanceFrom origin target
                |> Quantity.half

        targetCp =
            target |> Point2d.translateIn directionToTarget (Pixels.pixels (tileSize / 2))

        midpoint =
            Point2d.midpoint origin targetCp

        handleCp1 =
            Point2d.midpoint origin midpoint
                |> Point2d.translateIn (Direction2d.rotateClockwise directionToTarget) handleDistance

        handleCp2 =
            midpoint
                |> Point2d.translateIn (Direction2d.rotateCounterclockwise directionToTarget) handleDistance
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> CubicSpline2d.segments 16
        |> Polyline2d.vertices


statusDescription : Status -> String
statusDescription status =
    case status of
        Moving ->
            "Moving"

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
