module Coords exposing
    ( Coords
    , corner
    , cornerAndNeighbors
    , diagonalNeighbors
    , filterBy
    , float
    , next
    , parallelNeighbors
    , shiftTo
    , toString
    )

import Direction exposing (Corner(..), Direction(..))


type alias Coords =
    ( Int, Int )


type alias Positioned a =
    { a | coords : Coords }


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
diagonalNeighbors coords =
    [ corner coords TopLeft
    , corner coords TopRight
    , corner coords BottomLeft
    , corner coords BottomRight
    ]


parallelNeighbors : Coords -> List Coords
parallelNeighbors coords =
    List.map (next coords) Direction.all


corner : Coords -> Corner -> Coords
corner ( x, y ) c =
    case c of
        TopLeft ->
            ( x - 1, y - 1 )

        TopRight ->
            ( x + 1, y - 1 )

        BottomLeft ->
            ( x - 1, y + 1 )

        BottomRight ->
            ( x + 1, y + 1 )


{-| Corner plus natural neighbors (clockwise).

    e.g. Left, TopLeft, Up

-}
cornerAndNeighbors : Corner -> Coords -> List Coords
cornerAndNeighbors c coords =
    case c of
        TopLeft ->
            [ next coords Left, corner coords TopLeft, next coords Up ]

        TopRight ->
            [ next coords Up, corner coords TopRight, next coords Right ]

        BottomLeft ->
            [ next coords Down, corner coords BottomLeft, next coords Left ]

        BottomRight ->
            [ next coords Right, corner coords BottomRight, next coords Down ]


filterBy : List (Positioned a) -> Coords -> List (Positioned a)
filterBy thingsWithCoords coords =
    thingsWithCoords
        |> List.filter (\el -> el.coords == coords)


shiftTo : Int -> Coords -> Direction -> Coords
shiftTo distance ( x, y ) dir =
    case dir of
        Up ->
            ( x, y + distance )

        Right ->
            ( x + distance, y )

        Down ->
            ( x, y - distance )

        Left ->
            ( x - distance, y )


float : Coords -> ( Float, Float )
float ( x, y ) =
    ( toFloat x, toFloat y )


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
