module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , TurnKind(..)
    , isRespawning
    , isStoppedOrWaiting
    , isTurning
    , move
    , new
    , newWithHome
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
    = Sedan1
    | Sedan2
    | Sedan3
    | Sedan4
    | Sedan5


type Status
    = Moving
    | Turning TurnKind
    | WaitingForTrafficLights
    | StoppedAtIntersection
    | Yielding
    | Respawning
    | SkippingRound


type TurnKind
    = LeftTurn
    | RightTurn
    | UTurn


new : CarKind -> Car
new kind =
    Car ( 0, 0 ) Up kind Respawning Nothing


newWithHome : CarKind -> Coords -> Int -> Car
newWithHome kind homeCoords lotId =
    Car homeCoords Up kind SkippingRound (Just lotId)


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


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    case car.status of
        WaitingForTrafficLights ->
            True

        StoppedAtIntersection ->
            True

        Yielding ->
            True

        SkippingRound ->
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

        SkippingRound ->
            "Skipping the round"
