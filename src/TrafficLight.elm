module TrafficLight exposing
    ( TrafficLight
    , TrafficLightKind(..)
    , TrafficLights
    , advanceLight
    , advanceTimer
    , default
    , new
    )

import Cell exposing (OrthogonalDirection(..))


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , facing : OrthogonalDirection
    , timeRemaining : Int
    }


type alias TrafficLights =
    List TrafficLight


new : TrafficLightKind -> OrthogonalDirection -> TrafficLight
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
    [ new Green Up
    , new Green Down
    , new Red Left
    , new Red Right
    ]


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
