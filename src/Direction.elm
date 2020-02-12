module Direction exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left


allDirections : List Direction
allDirections =
    [ Up, Right, Down, Left ]


oppositeDirection : Direction -> Direction
oppositeDirection dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right
