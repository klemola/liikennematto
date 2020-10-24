module Cell exposing
    ( Cell
    , bottomLeftCorner
    , boundingBox
    , corner
    , cornerAndNeighbors
    , diagonalNeighbors
    , fromPosition
    , next
    , parallelNeighbors
    )

import Collision exposing (BoundingBox)
import Config exposing (boardSize, tileSize)
import Direction exposing (Corner(..), Direction(..))
import Position exposing (Position)


type alias Cell =
    ( Int, Int )


next : Cell -> Direction -> Cell
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


diagonalNeighbors : Cell -> List Cell
diagonalNeighbors position =
    [ corner position TopLeft
    , corner position TopRight
    , corner position BottomLeft
    , corner position BottomRight
    ]


parallelNeighbors : Cell -> List Cell
parallelNeighbors position =
    List.map (next position) Direction.all


corner : Cell -> Corner -> Cell
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
cornerAndNeighbors : Corner -> Cell -> List Cell
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


bottomLeftCorner : Cell -> Position
bottomLeftCorner ( cellX, cellY ) =
    let
        ( x, y ) =
            ( toFloat cellX, toFloat cellY )

        ( adjustedX, adjustedY ) =
            ( x - 1, toFloat boardSize - y )
    in
    if adjustedX < 0 || adjustedY < 0 then
        ( 0, 0 )

    else
        ( adjustedX * tileSize, adjustedY * tileSize )


fromPosition : Position -> Cell
fromPosition ( x, y ) =
    let
        boardSizeScaled =
            toFloat boardSize * tileSize
    in
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
