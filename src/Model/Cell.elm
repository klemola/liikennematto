module Model.Cell exposing
    ( Cell
    , CellCoordinates
    , bottomLeftCorner
    , boundingBox
    , centerPoint
    , coordinates
    , fromCoordinates
    , horizontalCellsAmount
    , nextOrthogonalCell
    , quadrantNeighbors
    , size
    , toString
    , verticalCellsAmount
    )

import Common
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Geometry
    exposing
        ( DiagonalDirection(..)
        , LMBoundingBox2d
        , LMPoint2d
        , OrthogonalDirection(..)
        )
import Point2d
import Quantity exposing (negativeInfinity)
import Vector2d


type Cell
    = Cell CellCoordinates


type alias CellCoordinates =
    ( Int, Int )


horizontalCellsAmount : Int
horizontalCellsAmount =
    10


verticalCellsAmount : Int
verticalCellsAmount =
    horizontalCellsAmount


size : Length
size =
    Length.meters 16


fromCoordinates : CellCoordinates -> Maybe Cell
fromCoordinates ( x, y ) =
    if
        isValidCoordinate x horizontalCellsAmount
            && isValidCoordinate y verticalCellsAmount
    then
        Just (Cell ( x, y ))

    else
        Nothing


isValidCoordinate : Int -> Int -> Bool
isValidCoordinate coordinate cellsAmount =
    coordinate > 0 && coordinate <= cellsAmount


coordinates : Cell -> CellCoordinates
coordinates (Cell coords) =
    coords


bottomLeftCorner : Cell -> LMPoint2d
bottomLeftCorner cell =
    let
        ( cellX, cellY ) =
            coordinates cell
    in
    if cellX > 0 && cellY > 0 then
        let
            ( xMultiplier, yMultiplier ) =
                ( toFloat (cellX - 1)
                , toFloat (verticalCellsAmount - cellY)
                )
        in
        Point2d.xy
            (size |> Quantity.multiplyBy xMultiplier)
            (size |> Quantity.multiplyBy yMultiplier)

    else
        -- When Cells are built from coordinates, the coordinates are required to be positive.
        -- Invalid input results in a fallback value that is guaranteed to be outside the tilemap.
        Point2d.xy negativeInfinity negativeInfinity


centerPoint : Cell -> LMPoint2d
centerPoint cell =
    let
        displacement =
            Vector2d.xy
                (Quantity.half size)
                (Quantity.half size)
    in
    bottomLeftCorner cell |> Point2d.translateBy displacement


boundingBox : Cell -> LMBoundingBox2d
boundingBox cell =
    Common.boundingBoxWithDimensions size size (bottomLeftCorner cell)


toString : Cell -> String
toString cell =
    let
        ( x, y ) =
            coordinates cell
    in
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"


nextOrthogonalCell : OrthogonalDirection -> Cell -> Maybe Cell
nextOrthogonalCell dir cell =
    let
        ( x, y ) =
            coordinates cell
    in
    case dir of
        Up ->
            fromCoordinates ( x, y - 1 )

        Right ->
            fromCoordinates ( x + 1, y )

        Down ->
            fromCoordinates ( x, y + 1 )

        Left ->
            fromCoordinates ( x - 1, y )


nextDiagonalCell : DiagonalDirection -> Cell -> Maybe Cell
nextDiagonalCell dir cell =
    let
        ( x, y ) =
            coordinates cell
    in
    case dir of
        TopLeft ->
            fromCoordinates ( x - 1, y - 1 )

        TopRight ->
            fromCoordinates ( x + 1, y - 1 )

        BottomLeft ->
            fromCoordinates ( x - 1, y + 1 )

        BottomRight ->
            fromCoordinates ( x + 1, y + 1 )


{-| Corner plus natural neighbors (clockwise).

    e.g. Left, TopLeft, Up

-}
quadrantNeighbors : DiagonalDirection -> Cell -> List Cell
quadrantNeighbors dir origin =
    case dir of
        TopLeft ->
            Maybe.values
                [ nextOrthogonalCell Left origin
                , nextDiagonalCell TopLeft origin
                , nextOrthogonalCell Up origin
                ]

        TopRight ->
            Maybe.values
                [ nextOrthogonalCell Up origin
                , nextDiagonalCell TopRight origin
                , nextOrthogonalCell Right origin
                ]

        BottomLeft ->
            Maybe.values
                [ nextOrthogonalCell Down origin
                , nextDiagonalCell BottomLeft origin
                , nextOrthogonalCell Left origin
                ]

        BottomRight ->
            Maybe.values
                [ nextOrthogonalCell Right origin
                , nextDiagonalCell BottomRight origin
                , nextOrthogonalCell Down origin
                ]
