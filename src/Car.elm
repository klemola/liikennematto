module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , TurnKind(..)
    , isConfused
    , isStoppedOrWaiting
    , isTurning
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

import Direction exposing (Direction(..))
import Position exposing (Position)
import RoadNetwork exposing (RNNodeContext)
import Tile exposing (Tile(..))


type alias Car =
    { position : Position
    , rotation : Float
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
    , route : List RNNodeContext
    }


type CarKind
    = SedanA
    | SedanB
    | SedanC
    | SedanD
    | SedanE


type Status
    = Moving
    | Turning TurnKind
    | WaitingForTrafficLights
    | StoppedAtIntersection
    | Yielding
    | ParkedAtLot
    | SkippingRound
    | Confused


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding, SkippingRound ]


type TurnKind
    = LeftTurn
    | RightTurn


new : CarKind -> Car
new kind =
    { position = ( 0, 0 )
    , rotation = Direction.toRadians Up
    , kind = kind
    , status = Confused
    , homeLotId = Nothing
    , route = []
    }


withHome : Int -> Car -> Car
withHome lotId car =
    { car | homeLotId = Just lotId }


isTurning : Car -> Bool
isTurning car =
    case car.status of
        Turning _ ->
            True

        _ ->
            False


isConfused : Car -> Bool
isConfused car =
    car.status == Confused


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    List.member car.status stoppedOrWaiting


move : Car -> Car
move car =
    let
        ( x, y ) =
            car.position

        ( addX, addY ) =
            fromPolar ( 1, car.rotation )
    in
    { car
        | position = ( x + addX, y + addY )
        , status = Moving
    }


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


turnDirection : Car -> Float -> TurnKind
turnDirection car target =
    if target == 0 || target > car.rotation then
        LeftTurn

    else
        RightTurn


statusDescription : Status -> String
statusDescription status =
    case status of
        Moving ->
            "Moving"

        Turning LeftTurn ->
            "Turning left"

        Turning RightTurn ->
            "Turning right"

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
