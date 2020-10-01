module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , TurnKind(..)
    , isParkedAtLot
    , isRespawning
    , isStoppedOrWaiting
    , isTurning
    , move
    , new
    , skipRound
    , spawn
    , statusDescription
    , stopAtIntersection
    , turn
    , waitForRespawn
    , waitForTrafficLights
    , yield
    )

import Coords exposing (Coords)
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))


type alias Car =
    { coords : Coords
    , direction : Direction
    , kind : CarKind
    , status : Status
    , homeLotId : Maybe Int
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
    | Respawning
    | ParkedAtLot
    | SkippingRound


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding, SkippingRound ]


type TurnKind
    = LeftTurn
    | RightTurn
    | UTurn


new : CarKind -> Car
new kind =
    Car ( 0, 0 ) Up kind Respawning Nothing


isTurning : Car -> Bool
isTurning car =
    case car.status of
        Turning _ ->
            True

        _ ->
            False


isRespawning : Car -> Bool
isRespawning car =
    car.status == Respawning


isParkedAtLot : Car -> Bool
isParkedAtLot car =
    car.status == ParkedAtLot


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    List.member car.status stoppedOrWaiting


move : Car -> Car
move car =
    let
        nextCoords =
            if isParkedAtLot car then
                -- implicitly move car to the street from the driveway
                Direction.previous car.direction
                    |> Coords.next car.coords

            else
                Coords.next car.coords car.direction
    in
    { car | coords = nextCoords, status = Moving }


skipRound : Car -> Car
skipRound car =
    { car | status = SkippingRound }


turn : Direction -> Car -> Car
turn nextDirection car =
    let
        turnDirection =
            if Direction.previous car.direction == nextDirection then
                LeftTurn

            else if Direction.next car.direction == nextDirection then
                RightTurn

            else
                UTurn
    in
    { car | direction = nextDirection, status = Turning turnDirection }


waitForTrafficLights : Car -> Car
waitForTrafficLights car =
    { car | status = WaitingForTrafficLights }


yield : Car -> Car
yield car =
    { car | status = Yielding }


stopAtIntersection : Car -> Car
stopAtIntersection car =
    { car | status = StoppedAtIntersection }


waitForRespawn : Car -> Car
waitForRespawn car =
    { car | status = Respawning, coords = ( 0, 0 ) }


spawn : Coords -> Car -> Car
spawn coords car =
    { car | status = SkippingRound, coords = coords }


statusDescription : Status -> String
statusDescription status =
    case status of
        Moving ->
            "Moving"

        Turning LeftTurn ->
            "Turning left"

        Turning RightTurn ->
            "Turning right"

        Turning UTurn ->
            "Making a U-turn"

        WaitingForTrafficLights ->
            "Stopped @ traffic lights"

        StoppedAtIntersection ->
            "Stopped"

        Yielding ->
            "Yielding"

        Respawning ->
            "Respawning"

        ParkedAtLot ->
            "Parked @ lot"

        SkippingRound ->
            "Skipping the round"
