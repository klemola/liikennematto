module Cell exposing
    ( Cell
    , Corner(..)
    , OrthogonalDirection(..)
    , allODs
    , bottomLeftCorner
    , boundingBox
    , center
    , cornerAndNeighbors
    , corners
    , crossOrthogonalDirection
    , down
    , isVertical
    , left
    , next
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , right
    , toString
    , up
    )

import Config exposing (boardSize, tileSizeInMeters)
import Direction2d
import Geometry exposing (LMBoundingBox2d, LMDirection2d, LMPoint2d)
import Point2d
import Quantity
import Vector2d


type alias Cell =
    ( Int, Int )



-- leftovers from Direction


type OrthogonalDirection
    = Up
    | Right
    | Down
    | Left


type Corner
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


horizontalODs : List OrthogonalDirection
horizontalODs =
    [ Left, Right ]


verticalODs : List OrthogonalDirection
verticalODs =
    [ Up, Down ]


allODs : List OrthogonalDirection
allODs =
    verticalODs ++ horizontalODs


corners : List Corner
corners =
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
            horizontalODs

        Right ->
            verticalODs

        Down ->
            horizontalODs

        Left ->
            verticalODs


isVertical : OrthogonalDirection -> Bool
isVertical direction =
    List.member direction verticalODs



-- compatibility layer


up =
    Direction2d.positiveY


right =
    Direction2d.positiveX


down =
    Direction2d.negativeY


left =
    Direction2d.negativeX


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



-- old logic


next : OrthogonalDirection -> Cell -> Cell
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


bottomLeftCorner : Cell -> LMPoint2d
bottomLeftCorner ( cellX, cellY ) =
    let
        ( x, y ) =
            ( toFloat (cellX - 1), toFloat (boardSize - cellY) )
    in
    if x < 0 || y < 0 then
        Point2d.origin

    else
        Point2d.xy
            (tileSizeInMeters |> Quantity.multiplyBy x)
            (tileSizeInMeters |> Quantity.multiplyBy y)


center : Cell -> LMPoint2d
center cell =
    let
        displacement =
            Vector2d.xy
                (Quantity.half tileSizeInMeters)
                (Quantity.half tileSizeInMeters)
    in
    bottomLeftCorner cell
        |> Point2d.translateBy displacement


boundingBox : Cell -> LMBoundingBox2d
boundingBox cell =
    bottomLeftCorner cell
        |> Geometry.boundingBoxWithDimensions tileSizeInMeters tileSizeInMeters


toString : Cell -> String
toString ( x, y ) =
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y
