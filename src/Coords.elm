module Coords exposing (..)

import Direction exposing (Direction(..))


type alias Coords =
    ( Int, Int )


next : Coords -> Direction -> Coords
next ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


diagonalNeighbors : Coords -> List Coords
diagonalNeighbors ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x + 1, y + 1 )
    ]


parallelNeighbors : Coords -> List Coords
parallelNeighbors coords =
    List.map (next coords) Direction.all


toString : Coords -> String
toString coords =
    let
        format n =
            n
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format (Tuple.first coords)
        , "y:"
        , format (Tuple.second coords)
        ]
