module Position exposing
    ( Position
    , difference
    , filterBy
    , shiftBy
    , toAngleRadians
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


shiftBy : Float -> Direction -> Position -> Position
shiftBy distance dir ( x, y ) =
    case dir of
        Up ->
            ( x, y + distance )

        Right ->
            ( x + distance, y )

        Down ->
            ( x, y - distance )

        Left ->
            ( x - distance, y )


difference : Position -> Position -> Position
difference ( fromX, fromY ) ( toX, toY ) =
    ( toX - fromX, toY - fromY )


toAngleRadians : Position -> Position -> Float
toAngleRadians origin destination =
    let
        ( diffX, diffY ) =
            difference origin destination

        angle =
            atan2 diffY diffX
    in
    if angle < 0 then
        angle + 2 * pi

    else
        angle


toString : Position -> String
toString position =
    let
        format n =
            n
                |> truncate
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format (Tuple.first position)
        , "y:"
        , format (Tuple.second position)
        ]
