module Direction exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


all : List Direction
all =
    [ Up, Right, Down, Left ]


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
