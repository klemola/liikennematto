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


nextCoords : Direction -> Coords -> Coords
nextCoords dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )
