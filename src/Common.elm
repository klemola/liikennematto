module Common exposing (..)


type alias Coords =
    ( Int, Int )


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


nextCoords : Coords -> Direction -> Coords
nextCoords ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )
