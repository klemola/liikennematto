module TrafficLight exposing
    ( TrafficLight
    , TrafficLightKind(..)
    , TrafficLights
    , advanceLight
    , advanceTimer
    , default
    , fromTrafficDirection
    , isGreen
    , new
    , trafficAllowedFromDirection
    )

import Direction exposing (Direction(..))


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , facing : Direction
    , timeRemaining : Int
    }


type alias TrafficLights =
    List TrafficLight


fromTrafficDirection : List Direction -> List TrafficLight
fromTrafficDirection direction =
    case direction of
        [ Up, Down ] ->
            [ new Green Up, new Green Down ]

        [ Left, Right ] ->
            [ new Red Left, new Red Right ]

        _ ->
            []


new : TrafficLightKind -> Direction -> TrafficLight
new kind facing =
    case kind of
        Green ->
            TrafficLight Green facing 7

        Yellow ->
            TrafficLight Yellow facing 1

        Red ->
            TrafficLight Red facing 8


default : List TrafficLight
default =
    Direction.byOrientation
        |> List.concatMap fromTrafficDirection


advanceTimer : TrafficLight -> TrafficLight
advanceTimer tl =
    { tl | timeRemaining = tl.timeRemaining - 1 }


advanceLight : TrafficLightKind -> TrafficLightKind
advanceLight tlKind =
    case tlKind of
        Green ->
            Yellow

        Yellow ->
            Red

        Red ->
            Green


isGreen : TrafficLight -> Bool
isGreen tl =
    case tl.kind of
        Green ->
            True

        _ ->
            False


trafficAllowedFromDirection : TrafficLights -> Direction -> Bool
trafficAllowedFromDirection trafficLights entryDirection =
    let
        signalXsEntryAllowed tl =
            isGreen tl && tl.facing == entryDirection
    in
    List.any signalXsEntryAllowed trafficLights
