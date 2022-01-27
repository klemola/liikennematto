module Model.Geometry exposing
    ( DiagonalDirection(..)
    , LMBoundingBox2d
    , LMDirection2d
    , LMEntityCoordinates
    , LMPoint2d
    , LMPolyline2d
    , OrthogonalDirection(..)
    , crossOrthogonalDirection
    , diagonalDirections
    , down
    , isVerticalDirection
    , left
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , orthogonalDirections
    , right
    , up
    )

import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Length
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Length.Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Length.Meters LMEntityCoordinates


type alias LMPolyline2d =
    Polyline2d Length.Meters LMEntityCoordinates



--
-- Logical directions (for the tilemap)
--
-- Room for improvement: union types for logical directions may not be necessary - plain Direction2d values could work.


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


up : Direction2d.Direction2d LMEntityCoordinates
up =
    Direction2d.positiveY


right : Direction2d.Direction2d LMEntityCoordinates
right =
    Direction2d.positiveX


down : Direction2d.Direction2d LMEntityCoordinates
down =
    Direction2d.negativeY


left : Direction2d.Direction2d LMEntityCoordinates
left =
    Direction2d.negativeX


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
