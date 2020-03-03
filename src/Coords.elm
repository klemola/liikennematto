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
    [ ( 5, 1 )
    , ( 1, 2 )
    , ( 2, 2 )
    , ( 3, 2 )
    , ( 4, 2 )
    , ( 6, 2 )
    , ( 7, 2 )
    , ( 8, 2 )
    , ( 1, 3 )
    , ( 5, 3 )
    , ( 8, 3 )
    , ( 1, 4 )
    , ( 5, 4 )
    , ( 8, 4 )
    , ( 2, 5 )
    , ( 3, 5 )
    , ( 4, 5 )
    , ( 6, 5 )
    , ( 7, 5 )
    , ( 8, 5 )
    , ( 1, 6 )
    , ( 5, 6 )
    , ( 1, 7 )
    , ( 5, 7 )
    , ( 1, 8 )
    , ( 5, 8 )
    ]


hasRoad : Coords -> Bool
hasRoad coords =
    List.member coords roads


seeRoadAhead : Coords -> Direction -> Bool
seeRoadAhead coords dir =
    let
        connections =
            roadConnections coords
    in
    List.any (\conn -> conn == next coords dir) connections


neighbors : Coords -> List Coords
neighbors coords =
    Direction.all
        |> List.map (next coords)


roadConnections : Coords -> List Coords
roadConnections coords =
    neighbors coords
        |> List.filter hasRoad


signalIntersections : List Coords
signalIntersections =
    [ ( 5, 2 ) ]


hasSignalIntersection : Coords -> Bool
hasSignalIntersection coords =
    List.member coords signalIntersections


yieldIntersections : List Coords
yieldIntersections =
    [ ( 1, 5 ) ]


hasYieldIntersection : Coords -> Bool
hasYieldIntersection coords =
    List.member coords yieldIntersections


stopIntersections : List Coords
stopIntersections =
    [ ( 5, 5 ) ]


hasStopIntersection : Coords -> Bool
hasStopIntersection coords =
    List.member coords stopIntersections
