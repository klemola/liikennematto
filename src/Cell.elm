module Cell exposing
    ( Cell
    , bottomLeftCorner
    , boundingBox
    , cornerAndNeighbors
    , fromPosition
    , next
    )

import Collision exposing (BoundingBox)
import Config exposing (boardSize, boardSizeScaled, tileSize)
import Direction exposing (Corner(..), Direction(..))
import Position exposing (Position)


type alias Cell =
    ( Int, Int )


next : Direction -> Cell -> Cell
next dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


corner : Corner -> Cell -> Cell
corner c ( x, y ) =
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
cornerAndNeighbors : Corner -> Cell -> List Cell
cornerAndNeighbors c position =
    case c of
        TopLeft ->
            [ next Left position, corner TopLeft position, next Up position ]

        TopRight ->
            [ next Up position, corner TopRight position, next Right position ]

        BottomLeft ->
            [ next Down position, corner BottomLeft position, next Left position ]

        BottomRight ->
            [ next Right position, corner BottomRight position, next Down position ]


bottomLeftCorner : Cell -> Position
bottomLeftCorner ( cellX, cellY ) =
    let
        ( x, y ) =
            ( toFloat (cellX - 1), toFloat (boardSize - cellY) )
    in
    if x < 0 || y < 0 then
        ( 0, 0 )

    else
        ( x * tileSize, y * tileSize )


fromPosition : Position -> Cell
fromPosition ( x, y ) =
    if x >= boardSizeScaled || y >= boardSizeScaled then
        ( 0, 0 )

    else
        ( floor ((x / tileSize) + 1), floor ((boardSizeScaled - y) / tileSize) )


boundingBox : Cell -> BoundingBox
boundingBox cell =
    let
        ( x, y ) =
            bottomLeftCorner cell
    in
    { x = x, y = y, width = tileSize, height = tileSize }
