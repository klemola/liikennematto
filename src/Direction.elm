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
