module Tilemap.Cell exposing
    ( AreaBounds
    , Cell
    , CellCoordinates
    , Constraints
    , array1DIndex
    , bottomLeftCorner
    , boundingBox
    , centerPoint
    , connectedBounds
    , coordinates
    , fromArea
    , fromArray1DIndex
    , fromArray1DIndexUnsafe
    , fromCoordinates
    , fromCoordinatesSet
    , fromCoordinatesUnsafe
    , isAdjacent
    , isIdentical
    , nextOrthogonalCell
    , nextOrthogonalCellUnsafe
    , orthogonalDirection
    , orthogonalNeighbors
    , placeIn
    , quadrantNeighbors
    , size
    , toString
    , translateBy
    , translateByVector
    )

import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates)
import Length exposing (Length)
import Lib.Bitmask exposing (OrthogonalMatch)
import Lib.DiagonalDirection exposing (DiagonalDirection(..))
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import Maybe.Extra as Maybe
import Point2d exposing (Point2d)
import Quantity exposing (Unitless, negativeInfinity)
import Set exposing (Set)
import Vector2d exposing (Vector2d)


type Cell
    = Cell CellProperties


type alias CellProperties =
    { x : Int
    , y : Int
    , bottomLeftCorner : Point2d Length.Meters GlobalCoordinates
    , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
    }


type alias CellCoordinates =
    ( Int, Int )


type alias Constraints a =
    -- The partial record definition allows one to pass a larger config record as constraints, for convenience
    { a
        | horizontalCellsAmount : Int
        , verticalCellsAmount : Int
    }


size : Length
size =
    Length.meters 16


minX : Int
minX =
    1


minY : Int
minY =
    1



--
-- Create new Cells
--


fromCoordinates : Constraints a -> CellCoordinates -> Maybe Cell
fromCoordinates constraints cellCoordinates =
    let
        ( x, y ) =
            cellCoordinates
    in
    if
        isValidCoordinate x constraints.horizontalCellsAmount
            && isValidCoordinate y constraints.verticalCellsAmount
    then
        Just (Cell (buildCellProperties constraints cellCoordinates))

    else
        Nothing


fromCoordinatesUnsafe : Constraints a -> CellCoordinates -> Cell
fromCoordinatesUnsafe constraints cellCoordinates =
    Cell (buildCellProperties constraints cellCoordinates)


buildCellProperties : Constraints a -> CellCoordinates -> CellProperties
buildCellProperties constraints ( x, y ) =
    let
        bottomLeftCornerPoint =
            if x > 0 && y > 0 then
                let
                    yMultiplier =
                        (constraints.verticalCellsAmount - y) |> toFloat

                    xMultiplier =
                        (x - 1) |> toFloat
                in
                Point2d.xy
                    (size |> Quantity.multiplyBy xMultiplier)
                    (size |> Quantity.multiplyBy yMultiplier)

            else
                -- When Cells are built from coordinates, the coordinates are required to be positive.
                -- Invalid input results in a fallback value that is guaranteed to be outside the tilemap.
                Point2d.xy negativeInfinity negativeInfinity
    in
    { x = x
    , y = y
    , bottomLeftCorner = bottomLeftCornerPoint
    , boundingBox = Common.boundingBoxWithDimensions size size bottomLeftCornerPoint
    }


fromCoordinatesSet : Constraints a -> Set CellCoordinates -> List Cell
fromCoordinatesSet constraints set =
    Set.foldl
        (\coords acc ->
            case fromCoordinates constraints coords of
                Just cell ->
                    cell :: acc

                Nothing ->
                    acc
        )
        []
        set


type alias AreaBounds =
    { minX : Int, maxX : Int, minY : Int, maxY : Int }


fromArea : Constraints a -> AreaBounds -> List Cell
fromArea constraints bounds =
    let
        xValues =
            List.range bounds.minX bounds.maxX

        yValues =
            List.range bounds.minY bounds.maxY
    in
    List.concatMap
        (\yVal ->
            List.filterMap
                (\xVal ->
                    fromCoordinates constraints ( xVal, yVal )
                )
                xValues
        )
        yValues


fromArray1DIndex : Constraints a -> Int -> Maybe Cell
fromArray1DIndex constraints idx =
    let
        coordinatesZeroIndexed =
            { x = idx |> remainderBy constraints.horizontalCellsAmount
            , y = idx // constraints.horizontalCellsAmount
            }

        yPadding =
            if coordinatesZeroIndexed.y == constraints.verticalCellsAmount then
                0

            else
                1
    in
    -- Cells are 1-indexed - map the coordinates to match
    fromCoordinates constraints
        ( coordinatesZeroIndexed.x + 1
        , coordinatesZeroIndexed.y + yPadding
        )


fromArray1DIndexUnsafe : Constraints a -> Int -> Cell
fromArray1DIndexUnsafe constraints idx =
    let
        coordinatesZeroIndexed =
            { x = idx |> remainderBy constraints.horizontalCellsAmount
            , y = idx // constraints.horizontalCellsAmount
            }

        yPadding =
            if coordinatesZeroIndexed.y == constraints.verticalCellsAmount then
                0

            else
                1
    in
    -- Cells are 1-indexed - map the coordinates to match
    fromCoordinatesUnsafe constraints
        ( coordinatesZeroIndexed.x + 1
        , coordinatesZeroIndexed.y + yPadding
        )


nextOrthogonalCell : Constraints a -> OrthogonalDirection -> Cell -> Maybe Cell
nextOrthogonalCell constraints dir cell =
    fromCoordinates
        constraints
        (nextCoordinates dir
            (coordinates cell)
        )


nextOrthogonalCellUnsafe : Constraints a -> OrthogonalDirection -> Cell -> Cell
nextOrthogonalCellUnsafe constraints dir cell =
    fromCoordinatesUnsafe
        constraints
        (nextCoordinates dir
            (coordinates cell)
        )


nextCoordinates : OrthogonalDirection -> CellCoordinates -> CellCoordinates
nextCoordinates dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


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


orthogonalNeighbors : Constraints a -> Cell -> List ( OrthogonalDirection, Cell )
orthogonalNeighbors constraints cell =
    List.filterMap
        (\dir -> nextOrthogonalCell constraints dir cell |> Maybe.map (Tuple.pair dir))
        OrthogonalDirection.all


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


{-| Safe translation by CellCoordinates displacement, guaranteed to fit into bounds or return Nothing
-}
translateBy : Constraints a -> CellCoordinates -> Cell -> Maybe Cell
translateBy constraints ( offsetX, offsetY ) (Cell cellProperties) =
    fromCoordinates constraints
        ( cellProperties.x + offsetX
        , cellProperties.y + offsetY
        )


{-| Raw translation by Vector2d displacement, allows out of bounds coordinates to be returned
-}
translateByVector : Vector2d Unitless cellCoordinates -> Cell -> CellCoordinates
translateByVector displacement (Cell cellProperties) =
    let
        { x, y } =
            Vector2d.toUnitless displacement
    in
    ( cellProperties.x + floor x
    , cellProperties.y + floor y
    )


{-| Given a global grid and (local) subgrid, place subgrid cell into global space relative to the global origin
-}
placeIn : Constraints a -> Cell -> Cell -> Maybe Cell
placeIn globalConstraints globalOrigin (Cell cellProperties) =
    let
        offset =
            ( cellProperties.x - 1, cellProperties.y - 1 )
    in
    translateBy globalConstraints offset globalOrigin



--
-- Queries & interop
--


isValidCoordinate : Int -> Int -> Bool
isValidCoordinate coordinate cellsAmount =
    coordinate > 0 && coordinate <= cellsAmount


{-| Defines the the connected bounds, if the cell is positioned at the edge of the bounding grid.
-}
connectedBounds : Constraints a -> Cell -> OrthogonalMatch
connectedBounds constraints (Cell cellProperties) =
    { up = cellProperties.y == minY
    , left = cellProperties.x == minX
    , right = cellProperties.x == constraints.horizontalCellsAmount
    , down = cellProperties.y == constraints.verticalCellsAmount
    }


coordinates : Cell -> CellCoordinates
coordinates (Cell cellProperties) =
    ( cellProperties.x, cellProperties.y )


bottomLeftCorner : Cell -> Point2d Length.Meters GlobalCoordinates
bottomLeftCorner (Cell cellProperties) =
    cellProperties.bottomLeftCorner


centerPoint : Cell -> Point2d Length.Meters GlobalCoordinates
centerPoint (Cell cellProperties) =
    let
        displacement =
            Vector2d.xy
                (Quantity.half size)
                (Quantity.half size)
    in
    cellProperties.bottomLeftCorner |> Point2d.translateBy displacement


array1DIndex : Constraints a -> Cell -> Int
array1DIndex constraints cell =
    let
        ( cellX, cellY ) =
            coordinates cell

        xyZeroIndexed =
            { x = cellX - 1
            , y = cellY - 1
            }
    in
    xyZeroIndexed.x + (xyZeroIndexed.y * constraints.horizontalCellsAmount)


boundingBox : Cell -> BoundingBox2d Length.Meters GlobalCoordinates
boundingBox (Cell cellProperties) =
    cellProperties.boundingBox


isIdentical : Cell -> Cell -> Bool
isIdentical (Cell cellA) (Cell cellB) =
    cellA.x == cellB.x && cellA.y == cellB.y


isAdjacent : Cell -> Cell -> Bool
isAdjacent (Cell cellA) (Cell cellB) =
    abs (cellA.x - cellB.x) + abs (cellA.y - cellB.y) == 1


orthogonalDirection : Cell -> Cell -> Maybe OrthogonalDirection
orthogonalDirection from to =
    if isIdentical from to then
        Nothing

    else
        let
            ( x0, y0 ) =
                coordinates from

            ( x1, y1 ) =
                coordinates to
        in
        if x0 /= x1 && y0 /= y1 then
            Nothing

        else
            let
                dir =
                    if x1 > x0 then
                        Right

                    else if y1 > y0 then
                        Down

                    else if y0 > y1 then
                        Up

                    else
                        Left
            in
            Just dir


toString : Cell -> String
toString cell =
    let
        ( x, y ) =
            coordinates cell
    in
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"
