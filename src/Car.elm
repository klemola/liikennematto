module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , TurnKind(..)
    , isMoving
    , isRespawning
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
    }


type CarKind
    = Sedan1
    | Sedan2
    | Sedan3
    | Sedan4
    | Sedan5


type Status
    = Moving
    | Turning TurnKind
    | WaitingForTrafficLights
    | StoppedAtIntersection Int
    | Yielding
    | Respawning
    | SkippingRound


type TurnKind
    = LeftTurn
    | RightTurn
    | UTurn


new : CarKind -> Car
new kind =
    Car ( 0, 0 ) Up kind Respawning


isTurning : Car -> Bool
isTurning car =
    case car.status of
        Turning _ ->
            True

        _ ->
            False


isRespawning : Car -> Bool
isRespawning car =
    case car.status of
        Respawning ->
            True

        _ ->
            False


isMoving : Car -> Bool
isMoving car =
    case car.status of
        Moving ->
            True

        Respawning ->
            True

        Turning _ ->
            True

        _ ->
            False


move : Car -> Car
move car =
    let
        nextCoords =
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


stopAtIntersection : Int -> Car -> Car
stopAtIntersection roundsRemaining car =
    { car | status = StoppedAtIntersection roundsRemaining }


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

        StoppedAtIntersection roundsRemaining ->
            "Stopped..." ++ String.fromInt roundsRemaining

        Yielding ->
            "Yielding"

        Respawning ->
            "Respawning"

        SkippingRound ->
            "Skipping the round"
