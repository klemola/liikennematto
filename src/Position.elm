module Position exposing
    ( Position
    , corner
    , cornerAndNeighbors
    , diagonalNeighbors
    , filterBy
    , fromInt
    , next
    , parallelNeighbors
    , shiftTo
    , toString
    )

import Direction exposing (Corner(..), Direction(..))


type alias Position =
    ( Float, Float )


type alias Positioned a =
    { a | position : Position }


next : Position -> Direction -> Position
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


diagonalNeighbors : Position -> List Position
diagonalNeighbors position =
    [ corner position TopLeft
    , corner position TopRight
    , corner position BottomLeft
    , corner position BottomRight
    ]


parallelNeighbors : Position -> List Position
parallelNeighbors position =
    List.map (next position) Direction.all


corner : Position -> Corner -> Position
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
cornerAndNeighbors : Corner -> Position -> List Position
cornerAndNeighbors c position =
    case c of
        TopLeft ->
            [ next position Left, corner position TopLeft, next position Up ]

        TopRight ->
            [ next position Up, corner position TopRight, next position Right ]

        BottomLeft ->
            [ next position Down, corner position BottomLeft, next position Left ]

        BottomRight ->
            [ next position Right, corner position BottomRight, next position Down ]


filterBy : List (Positioned a) -> Position -> List (Positioned a)
filterBy thingsWithPosition position =
    thingsWithPosition
        |> List.filter (\el -> el.position == position)


shiftTo : Float -> Position -> Direction -> Position
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


fromInt : ( Int, Int ) -> Position
fromInt ( x, y ) =
    ( toFloat x, toFloat y )


toString : Position -> String
toString position =
    let
        format n =
            n
                |> String.fromFloat
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format (Tuple.first position)
        , "y:"
        , format (Tuple.second position)
        ]
