module Model.Cell exposing
    ( Cell
    , CellCoordinates
    , Constraints
    , bottomLeftCorner
    , boundingBox
    , centerPoint
    , coordinates
    , fromCoordinates
    , nextOrthogonalCell
    , quadrantNeighbors
    , size
    , toString
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


type alias Constraints a =
    -- The partial record definition allows one to pass a larger config record as constraints, for convenience
    { a
        | verticalCellsAmount : Int
        , horizontalCellsAmount : Int
    }


size : Length
size =
    Length.meters 16


fromCoordinates : Constraints a -> CellCoordinates -> Maybe Cell
fromCoordinates { horizontalCellsAmount, verticalCellsAmount } ( x, y ) =
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


bottomLeftCorner : Constraints a -> Cell -> LMPoint2d
bottomLeftCorner constraints cell =
    let
        ( cellX, cellY ) =
            coordinates cell
    in
    if cellX > 0 && cellY > 0 then
        let
            ( xMultiplier, yMultiplier ) =
                ( toFloat (cellX - 1)
                , toFloat (constraints.verticalCellsAmount - cellY)
                )
        in
        Point2d.xy
            (size |> Quantity.multiplyBy xMultiplier)
            (size |> Quantity.multiplyBy yMultiplier)

    else
        -- When Cells are built from coordinates, the coordinates are required to be positive.
        -- Invalid input results in a fallback value that is guaranteed to be outside the tilemap.
        Point2d.xy negativeInfinity negativeInfinity


centerPoint : Constraints a -> Cell -> LMPoint2d
centerPoint constraints cell =
    let
        displacement =
            Vector2d.xy
                (Quantity.half size)
                (Quantity.half size)
    in
    bottomLeftCorner constraints cell |> Point2d.translateBy displacement


boundingBox : Constraints a -> Cell -> LMBoundingBox2d
boundingBox constraints cell =
    Common.boundingBoxWithDimensions size size (bottomLeftCorner constraints cell)


toString : Cell -> String
toString cell =
    let
        ( x, y ) =
            coordinates cell
    in
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"


nextOrthogonalCell : Constraints a -> OrthogonalDirection -> Cell -> Maybe Cell
nextOrthogonalCell constraints dir cell =
    let
        ( x, y ) =
            coordinates cell
    in
    case dir of
        Up ->
            fromCoordinates constraints ( x, y - 1 )

        Right ->
            fromCoordinates constraints ( x + 1, y )

        Down ->
            fromCoordinates constraints ( x, y + 1 )

        Left ->
            fromCoordinates constraints ( x - 1, y )


nextDiagonalCell : Constraints a -> DiagonalDirection -> Cell -> Maybe Cell
nextDiagonalCell constraints dir cell =
    let
        ( x, y ) =
            coordinates cell
    in
    case dir of
        TopLeft ->
            fromCoordinates constraints ( x - 1, y - 1 )

        TopRight ->
            fromCoordinates constraints ( x + 1, y - 1 )

        BottomLeft ->
            fromCoordinates constraints ( x - 1, y + 1 )

        BottomRight ->
            fromCoordinates constraints ( x + 1, y + 1 )


{-| Corner plus natural neighbors (clockwise).

    e.g. Left, TopLeft, Up

-}
quadrantNeighbors : Constraints a -> DiagonalDirection -> Cell -> List Cell
quadrantNeighbors constraints dir origin =
    case dir of
        TopLeft ->
            Maybe.values
                [ nextOrthogonalCell constraints Left origin
                , nextDiagonalCell constraints TopLeft origin
                , nextOrthogonalCell constraints Up origin
                ]

        TopRight ->
            Maybe.values
                [ nextOrthogonalCell constraints Up origin
                , nextDiagonalCell constraints TopRight origin
                , nextOrthogonalCell constraints Right origin
                ]

        BottomLeft ->
            Maybe.values
                [ nextOrthogonalCell constraints Down origin
                , nextDiagonalCell constraints BottomLeft origin
                , nextOrthogonalCell constraints Left origin
                ]

        BottomRight ->
            Maybe.values
                [ nextOrthogonalCell constraints Right origin
                , nextDiagonalCell constraints BottomRight origin
                , nextOrthogonalCell constraints Down origin
                ]
