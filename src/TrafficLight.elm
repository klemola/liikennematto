module TrafficLight exposing (..)

import Collage exposing (..)
import Color exposing (Color)
import Direction exposing (Direction(..))


type TrafficLightKind
    = Red
    | Yellow
    | Green


type alias TrafficLight =
    { kind : TrafficLightKind
    , direction : Direction
    , timeRemaining : Int
    }


type alias TrafficLights =
    List TrafficLight


initial : Direction -> TrafficLight
initial dir =
    new Green dir


new : TrafficLightKind -> Direction -> TrafficLight
new kind dir =
    case kind of
        Green ->
            TrafficLight Green dir 5

        Yellow ->
            TrafficLight Yellow dir 3

        Red ->
            TrafficLight Red dir 3


advanceTimer : TrafficLight -> TrafficLight
advanceTimer tl =
    { tl | timeRemaining = tl.timeRemaining - 1 }


advanceLight : TrafficLightKind -> TrafficLightKind
advanceLight tlKind =
    case tlKind of
        Green ->
            Red

        Yellow ->
            Green

        Red ->
            Yellow


next : TrafficLight -> TrafficLight
next tl =
    if tl.timeRemaining == 0 then
        new (advanceLight tl.kind) tl.direction

    else
        advanceTimer tl


isGreen : TrafficLight -> Bool
isGreen tl =
    case tl.kind of
        Green ->
            True

        _ ->
            False


toColor : TrafficLightKind -> Color
toColor tlKind =
    case tlKind of
        Green ->
            Color.green

        Yellow ->
            Color.yellow

        Red ->
            Color.red


view : Float -> TrafficLight -> Collage msg
view blockSize tl =
    let
        centerToEdgeDistance =
            blockSize / 2

        -- These coordinates can be later replaced with Collage built-in layout tools
        topLeft =
            ( -centerToEdgeDistance, centerToEdgeDistance )

        topRight =
            ( centerToEdgeDistance, centerToEdgeDistance )

        bottomLeft =
            ( -centerToEdgeDistance, -centerToEdgeDistance )

        bottomRight =
            ( centerToEdgeDistance, -centerToEdgeDistance )

        -- This could also be a line that's rotated and then aligned to a side
        border start end =
            segment start end
                |> traced (solid thick (uniform (toColor tl.kind)))

        topBorder =
            border topLeft topRight

        rightBorder =
            border topRight bottomRight

        bottomBorder =
            border bottomLeft bottomRight

        leftBorder =
            border topLeft bottomLeft
    in
    case tl.direction of
        Up ->
            topBorder

        Right ->
            rightBorder

        Down ->
            bottomBorder

        Left ->
            leftBorder
