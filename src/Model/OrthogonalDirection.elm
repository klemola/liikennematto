module Model.OrthogonalDirection exposing
    ( DiagonalDirection(..)
    , OrthogonalDirection(..)
    , crossOrthogonalDirection
    , diagonalDirections
    , isVerticalDirection
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , orthogonalDirections
    )

import Model.Geometry
    exposing
        ( LMDirection2d
        , down
        , left
        , right
        , up
        )


type OrthogonalDirection
    = Up
    | Right
    | Down
    | Left


type DiagonalDirection
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


horizontalOrthogonalDirections : List OrthogonalDirection
horizontalOrthogonalDirections =
    [ Left, Right ]


verticalOrthogonalDirections : List OrthogonalDirection
verticalOrthogonalDirections =
    [ Up, Down ]


orthogonalDirections : List OrthogonalDirection
orthogonalDirections =
    verticalOrthogonalDirections ++ horizontalOrthogonalDirections


diagonalDirections : List DiagonalDirection
diagonalDirections =
    [ TopLeft, TopRight, BottomLeft, BottomRight ]


oppositeOrthogonalDirection : OrthogonalDirection -> OrthogonalDirection
oppositeOrthogonalDirection dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


crossOrthogonalDirection : OrthogonalDirection -> List OrthogonalDirection
crossOrthogonalDirection fromDir =
    case fromDir of
        Up ->
            horizontalOrthogonalDirections

        Right ->
            verticalOrthogonalDirections

        Down ->
            horizontalOrthogonalDirections

        Left ->
            verticalOrthogonalDirections


isVerticalDirection : OrthogonalDirection -> Bool
isVerticalDirection direction =
    List.member direction verticalOrthogonalDirections


orthogonalDirectionToLmDirection : OrthogonalDirection -> LMDirection2d
orthogonalDirectionToLmDirection dir =
    case dir of
        Up ->
            up

        Right ->
            right

        Down ->
            down

        Left ->
            left
