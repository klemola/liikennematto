module Direction exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


horizontal : List Direction
horizontal =
    [ Left, Right ]


vertical : List Direction
vertical =
    [ Up, Down ]


all : List Direction
all =
    vertical ++ horizontal


orientations : List (List Direction)
orientations =
    [ vertical, horizontal ]


opposite : Direction -> Direction
opposite dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


previous : Direction -> Direction
previous dir =
    case dir of
        Up ->
            Left

        Right ->
            Up

        Down ->
            Right

        Left ->
            Down


next : Direction -> Direction
next dir =
    case dir of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


cross : Direction -> List Direction
cross fromDir =
    case fromDir of
        Up ->
            horizontal

        Right ->
            vertical

        Down ->
            horizontal

        Left ->
            vertical


rotationDegrees : Direction -> Float
rotationDegrees dir =
    -- the values here are based on Collage logic: counter-clockwise rotation (from "Up")
    case dir of
        Up ->
            0

        Right ->
            270

        Down ->
            180

        Left ->
            90
