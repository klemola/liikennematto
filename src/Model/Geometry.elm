module Model.Geometry exposing
    ( DiagonalDirection(..)
    , GlobalCoordinates
    , LMArc2d
    , LMBoundingBox2d
    , LMCubicSpline2d
    , LMCubicSpline2dLocal
    , LMDirection2d
    , LMDirection2dLocal
    , LMFrame2d
    , LMLineSegment2d
    , LMPoint2d
    , LMPoint2dLocal
    , LMShape2d
    , LocalCoordinates
    , OrthogonalDirection(..)
    , crossOrthogonalDirection
    , diagonalDirections
    , down
    , left
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , orthogonalDirectionToString
    , orthogonalDirections
    , right
    , up
    )

import Arc2d exposing (Arc2d)
import BoundingBox2d exposing (BoundingBox2d)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type alias LMPoint2d =
    Point2d Length.Meters GlobalCoordinates


type alias LMPoint2dLocal =
    Point2d Length.Meters LocalCoordinates


type alias LMDirection2d =
    Direction2d GlobalCoordinates


type alias LMDirection2dLocal =
    Direction2d LocalCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Length.Meters GlobalCoordinates


type alias LMCubicSpline2d =
    CubicSpline2d Length.Meters GlobalCoordinates


type alias LMCubicSpline2dLocal =
    CubicSpline2d Length.Meters LocalCoordinates


type alias LMLineSegment2d =
    LineSegment2d Length.Meters GlobalCoordinates


type alias LMShape2d =
    Polygon2d Length.Meters GlobalCoordinates


type alias LMArc2d =
    Arc2d Length.Meters GlobalCoordinates


type alias LMFrame2d =
    Frame2d Length.Meters GlobalCoordinates { defines : LocalCoordinates }



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


up : Direction2d.Direction2d coordinates
up =
    Direction2d.positiveY


right : Direction2d.Direction2d coordinates
right =
    Direction2d.positiveX


down : Direction2d.Direction2d coordinates
down =
    Direction2d.negativeY


left : Direction2d.Direction2d coordinates
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


orthogonalDirectionToLmDirection : OrthogonalDirection -> Direction2d.Direction2d coordinates
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


orthogonalDirectionToString : OrthogonalDirection -> String
orthogonalDirectionToString dir =
    let
        dirString =
            case dir of
                Up ->
                    "up"

                Right ->
                    "right"

                Down ->
                    "down"

                Left ->
                    "left"
    in
    "OrthogonalDirection " ++ dirString
