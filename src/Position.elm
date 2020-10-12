module Position exposing
    ( Position
    , filterBy
    , shiftBy
    , toString
    )

import Direction exposing (Direction(..))


type alias Position =
    ( Float, Float )


type alias Positioned a =
    { a | position : Position }


filterBy : List (Positioned a) -> Position -> List (Positioned a)
filterBy thingsWithPosition position =
    List.filter (\el -> el.position == position) thingsWithPosition


shiftBy : Float -> Position -> Direction -> Position
shiftBy distance ( x, y ) dir =
    case dir of
        Up ->
            ( x, y + distance )

        Right ->
            ( x + distance, y )

        Down ->
            ( x, y - distance )

        Left ->
            ( x - distance, y )


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
