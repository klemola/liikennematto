module Car exposing
    ( Car
    , CarKind(..)
    , Status(..)
    , TurnKind(..)
    , isRespawning
    , isStoppedOrWaiting
    , isTurning
    , markAsConfused
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
    | Respawning
    | ParkedAtLot
    | SkippingRound
    | Confused


stoppedOrWaiting : List Status
stoppedOrWaiting =
    [ WaitingForTrafficLights, StoppedAtIntersection, Yielding, SkippingRound ]


type TurnKind
    = LeftTurn
    | RightTurn
    | UTurn


new : CarKind -> Car
new kind =
    { position = ( 0, 0 )
    , rotation = Direction.toRadians Up
    , kind = kind
    , status = Respawning
    , homeLotId = Nothing
    , route = []
    }


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


isStoppedOrWaiting : Car -> Bool
isStoppedOrWaiting car =
    List.member car.status stoppedOrWaiting


move : Car -> Car
move car =
    let
        ( x, y ) =
            car.position

        rotationInPolar =
            car.rotation + degrees 90

        rotationWithLimit =
            -- adjust rotation for pre-polar adjusted value of < 270°
            if rotationInPolar - degrees 360 >= 0 then
                rotationInPolar - degrees 360

            else
                rotationInPolar

        ( addX, addY ) =
            fromPolar ( 1, rotationWithLimit )
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
    let
        ( nextRotation, rotationComplete ) =
            rotateImmediatelyInPlace
                { target = target
                , currentRotation = car.rotation
                }

        turnDirection =
            if target == 0 || target > nextRotation then
                LeftTurn

            else
                RightTurn

        nextStatus =
            if rotationComplete then
                Moving

            else
                Turning turnDirection
    in
    { car
        | rotation = nextRotation
        , status = nextStatus
    }


rotateImmediatelyInPlace :
    { target : Float
    , currentRotation : Float
    }
    -> ( Float, Bool )
rotateImmediatelyInPlace { target, currentRotation } =
    let
        foo =
            Debug.log "current / target" ( currentRotation, target )
    in
    ( target
    , True
    )


rotateIncrementallyInPlace :
    { target : Float
    , currentRotation : Float
    }
    -> ( Float, Bool )
rotateIncrementallyInPlace { target, currentRotation } =
    let
        rotationPerStep =
            0.05

        rotationAfterStep =
            if target == 0 then
                min (degrees 360) (currentRotation + rotationPerStep)

            else
                min target (currentRotation + rotationPerStep)

        nextRotation =
            if rotationAfterStep >= degrees 360 then
                0

            else
                rotationAfterStep

        foo =
            Debug.log "rotation / target" ( nextRotation, target )
    in
    ( nextRotation
    , nextRotation == target || nextRotation == 0
    )


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
    { car | status = Respawning, position = ( 0, 0 ) }


spawn : Position -> Car -> Car
spawn position car =
    { car | status = SkippingRound, position = position }


markAsConfused : Car -> Car
markAsConfused car =
    { car | status = Confused }


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

        Confused ->
            "Confused"
