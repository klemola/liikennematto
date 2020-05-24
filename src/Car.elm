module Car exposing (Car, CarKind(..), Msg(..), Status(..), TurnKind(..), isRespawning, isTurning, new, statusDescription, update)

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
    | Waiting
    | StoppedAtIntersection Int
    | Yielding
    | Respawning


type TurnKind
    = LeftTurn
    | RightTurn
    | UTurn


type alias Model =
    Car


type Msg
    = Move
    | Turn Direction
    | Wait
    | YieldAtIntersection
    | StopAtIntersection Int
    | WaitForRespawn
    | Spawn Coords


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


update : Msg -> Model -> Model
update msg car =
    case msg of
        Move ->
            let
                nextCoords =
                    Coords.next car.coords car.direction
            in
            { car | coords = nextCoords, status = Moving }

        Turn nextDirection ->
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

        Wait ->
            { car | status = Waiting }

        YieldAtIntersection ->
            { car | status = Yielding }

        StopAtIntersection roundsRemaining ->
            { car | status = StoppedAtIntersection roundsRemaining }

        WaitForRespawn ->
            { car | status = Respawning, coords = ( 0, 0 ) }

        Spawn coords ->
            { car | status = Waiting, coords = coords }


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

        Waiting ->
            "Waiting"

        StoppedAtIntersection roundsRemaining ->
            "Stopped..." ++ String.fromInt roundsRemaining

        Yielding ->
            "Yielding"

        Respawning ->
            "Respawning"
