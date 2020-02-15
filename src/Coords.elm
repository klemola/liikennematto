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


roads : List Coords
roads =
    [ ( 1, 1 )
    , ( 2, 1 )
    , ( 5, 1 )
    , ( 2, 2 )
    , ( 3, 2 )
    , ( 4, 2 )
    , ( 6, 2 )
    , ( 7, 2 )
    , ( 8, 2 )
    , ( 5, 3 )
    , ( 8, 3 )
    , ( 5, 4 )
    , ( 8, 4 )
    , ( 1, 5 )
    , ( 2, 5 )
    , ( 3, 5 )
    , ( 4, 5 )
    , ( 6, 5 )
    , ( 7, 5 )
    , ( 8, 5 )
    , ( 5, 6 )
    , ( 5, 7 )
    , ( 5, 8 )
    ]


hasRoad : Coords -> Bool
hasRoad coords =
    List.member coords roads


roadConnections : Coords -> List Coords
roadConnections coords =
    Direction.all
        |> List.map (next coords)
        |> List.filter hasRoad


intersections : List Coords
intersections =
    [ ( 5, 2 ), ( 5, 5 ) ]


hasIntersection : Coords -> Bool
hasIntersection coords =
    List.member coords intersections
