module Position exposing
    ( Position
    , filterBy
    , logicalShiftBy
    , toString
    , visualShiftBy
    )

import Direction exposing (Direction(..))


type alias Position =
    ( Float, Float )


type alias Positioned a =
    { a | position : Position }


filterBy : List (Positioned a) -> Position -> List (Positioned a)
filterBy thingsWithPosition position =
    List.filter (\el -> el.position == position) thingsWithPosition


visualShiftBy : Float -> Position -> Direction -> Position
visualShiftBy distance ( x, y ) dir =
    case dir of
        Up ->
            ( x, y + distance )

        Right ->
            ( x + distance, y )

        Down ->
            ( x, y - distance )

        Left ->
            ( x - distance, y )


{-| Temporary workaround for non-obvious y coordinate direction in the car position
-}
logicalShiftBy : Float -> Position -> Direction -> Position
logicalShiftBy distance ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - distance )

        Right ->
            ( x + distance, y )

        Down ->
            ( x, y + distance )

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
