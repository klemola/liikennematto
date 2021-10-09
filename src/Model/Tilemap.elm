module Model.Tilemap exposing
    ( Cell
    , DiagonalDirection(..)
    , OrthogonalDirection(..)
    , Tile
    , TileChange(..)
    , Tilemap
    , TilemapChange
    , addTile
    , boundingBox
    , cellBottomLeftCorner
    , cellBoundingBox
    , cellCenterPoint
    , cellFromCoordinates
    , cellToString
    , cornerCells
    , curveTopLeft
    , diagonalDirections
    , empty
    , exists
    , horizontalRoad
    , inBounds
    , intersectionTDown
    , intersects
    , isCurve
    , isDeadend
    , isIntersection
    , isVerticalDirection
    , mapSize
    , nextOrthogonalCell
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , orthogonalDirections
    , potentialConnections
    , removeTile
    , rowsAndColumnsAmount
    , size
    , tileAt
    , tileSize
    , toList
    , verticalRoad
    )

import Array exposing (Array)
import BoundingBox2d
import Common
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMDirection2d
        , LMPoint2d
        , down
        , left
        , pixelsToMeters
        , right
        , up
        )
import Point2d
import Quantity exposing (negativeInfinity)
import Set exposing (Set)
import Vector2d


type Tilemap
    = Tilemap (Array Tile)


type alias Tile =
    Int


type Cell
    = Cell CellCoordinates


type alias CellCoordinates =
    ( Int, Int )


type alias TilemapChange =
    { nextTilemap : Tilemap
    , changedCells : ChangedCells
    }


type alias ChangedCells =
    List ( Cell, TileChange )


type TileChange
    = Add
    | Remove
    | Change


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



--
-- Tilemap
--


tileSize : Length
tileSize =
    pixelsToMeters 80


rowsAndColumnsAmount : Int
rowsAndColumnsAmount =
    10


mapSize : Length
mapSize =
    tileSize |> Quantity.multiplyBy (toFloat rowsAndColumnsAmount)


boundingBox : LMBoundingBox2d
boundingBox =
    Common.boundingBoxWithDimensions mapSize mapSize Point2d.origin


empty : Tilemap
empty =
    let
        arrSize =
            rowsAndColumnsAmount * rowsAndColumnsAmount
    in
    Tilemap (Array.initialize arrSize (always emptyTile))


addTile : Cell -> Tilemap -> TilemapChange
addTile cell tilemap =
    applyMask cell Add tilemap


removeTile : Cell -> Tilemap -> TilemapChange
removeTile cell tilemap =
    applyMask cell Remove tilemap


tileAt : Tilemap -> Cell -> Maybe Tile
tileAt (Tilemap tilemapContents) cell =
    let
        idx =
            indexFromCell cell
    in
    Array.get idx tilemapContents
        |> Maybe.andThen
            (\tile ->
                if tile == emptyTile then
                    Nothing

                else
                    Just tile
            )


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


intersects : LMBoundingBox2d -> Tilemap -> Bool
intersects testBB tilemap =
    toList (\cell _ -> cellBoundingBox cell) tilemap
        |> List.any (Common.boundingBoxOverlaps testBB)


exists : Cell -> Tilemap -> Bool
exists cell tilemap =
    tileAt tilemap cell |> Maybe.isJust


toList : (Cell -> Tile -> a) -> Tilemap -> List a
toList mapperFn (Tilemap tilemapContents) =
    let
        -- Keep track of the array index, which Array.foldl does not
        initialAcc =
            { acc = []
            , index = 0
            }

        mappedAcc =
            -- This is an optimization - Array.indexedMap would require double iteration (cell mapping + Nothing values discarded)
            Array.foldl
                (\tile { acc, index } ->
                    { acc =
                        if tile == emptyTile then
                            acc

                        else
                            cellFromIndex index
                                |> Maybe.map (\cell -> mapperFn cell tile :: acc)
                                |> Maybe.withDefault acc
                    , index = index + 1
                    }
                )
                initialAcc
                tilemapContents
    in
    mappedAcc.acc


size : Tilemap -> Int
size (Tilemap tilemapContents) =
    Array.foldl
        (\tile count ->
            if tile /= emptyTile then
                count + 1

            else
                count
        )
        0
        tilemapContents



--
-- Bit mask
--


applyMask : Cell -> TileChange -> Tilemap -> TilemapChange
applyMask origin tileChange tilemap =
    let
        originTile =
            tileAt tilemap origin |> Maybe.withDefault emptyTile

        potentiallyChangedCells =
            Maybe.values
                [ Just ( origin, originTile )
                , nextOrthogonalTile Up origin tilemap
                , nextOrthogonalTile Left origin tilemap
                , nextOrthogonalTile Right origin tilemap
                , nextOrthogonalTile Down origin tilemap
                ]

        acc =
            TilemapChange tilemap []
    in
    List.foldl
        (\( cellToCheck, currentTile ) { nextTilemap, changedCells } ->
            let
                nextTile =
                    if cellToCheck == origin && tileChange == Remove then
                        emptyTile

                    else
                        chooseTile nextTilemap cellToCheck

                cellChange =
                    CellChange cellToCheck currentTile nextTile
            in
            { nextTilemap = nextTilemap |> replaceTile cellToCheck nextTile
            , changedCells = updateChangedCells cellChange changedCells
            }
        )
        acc
        potentiallyChangedCells


type alias CellChange =
    { cell : Cell
    , currentTile : Tile
    , nextTile : Tile
    }


updateChangedCells : CellChange -> ChangedCells -> ChangedCells
updateChangedCells { cell, currentTile, nextTile } changedCells =
    let
        tileChange =
            if currentTile == emptyTile && nextTile /= emptyTile then
                Just Add

            else if currentTile /= emptyTile && nextTile == emptyTile then
                Just Remove

            else if currentTile /= nextTile then
                Just Change

            else
                Nothing
    in
    case tileChange of
        Just change ->
            ( cell, change ) :: changedCells

        Nothing ->
            changedCells


nextOrthogonalTile : OrthogonalDirection -> Cell -> Tilemap -> Maybe ( Cell, Tile )
nextOrthogonalTile dir cell tilemap =
    let
        maybeCell =
            nextOrthogonalCell dir cell

        maybeTile =
            maybeCell |> Maybe.andThen (tileAt tilemap)
    in
    Maybe.map2 Tuple.pair
        maybeCell
        maybeTile


chooseTile : Tilemap -> Cell -> Tile
chooseTile tilemap origin =
    let
        orthogonalNeighbors =
            { up = hasOrthogonalNeighborAt Up origin tilemap
            , left = hasOrthogonalNeighborAt Left origin tilemap
            , right = hasOrthogonalNeighborAt Right origin tilemap
            , down = hasOrthogonalNeighborAt Down origin tilemap
            }
    in
    fourBitBitmask orthogonalNeighbors


hasOrthogonalNeighborAt : OrthogonalDirection -> Cell -> Tilemap -> Bool
hasOrthogonalNeighborAt dir cell tilemap =
    nextOrthogonalCell dir cell |> Maybe.unwrap False (\neighborCell -> exists neighborCell tilemap)


replaceTile : Cell -> Tile -> Tilemap -> Tilemap
replaceTile cell tile (Tilemap tilemapContents) =
    let
        idx =
            indexFromCell cell

        tiles =
            tilemapContents |> Array.set idx tile
    in
    Tilemap tiles


type alias OrthogonalNeighbors =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    }


{-| Calculates tile number (ID) based on surrounding tiles

    Up = 2^0 = 1
    Left = 2^1 = 2
    Right = 2^2 = 4
    Down = 2^3 = 8

    e.g. tile bordered by tiles in Up and Right directions 1*1 + 2*0 + 4*1 + 8*0 = 0101 = 5

-}
fourBitBitmask : OrthogonalNeighbors -> Int
fourBitBitmask { up, left, right, down } =
    1 * boolToBinary up + 2 * boolToBinary left + 4 * boolToBinary right + 8 * boolToBinary down


boolToBinary : Bool -> Int
boolToBinary booleanValue =
    if booleanValue then
        1

    else
        0



-- Cells


cellFromIndex : Int -> Maybe Cell
cellFromIndex idx =
    let
        xyZeroIndexed =
            { x = remainderBy rowsAndColumnsAmount idx
            , y = idx // rowsAndColumnsAmount
            }
    in
    -- Cells are 1-indexed - map the coordinates to match
    cellFromCoordinates ( xyZeroIndexed.x + 1, xyZeroIndexed.y + 1 )


indexFromCell : Cell -> Int
indexFromCell (Cell ( x, y )) =
    let
        xyZeroIndexed =
            { x = x - 1
            , y = y - 1
            }
    in
    -- Arrays are 0-indexed - map the coordinates to match
    xyZeroIndexed.x + (xyZeroIndexed.y * rowsAndColumnsAmount)


cellFromCoordinates : CellCoordinates -> Maybe Cell
cellFromCoordinates ( x, y ) =
    if isValidCoordinate x && isValidCoordinate y then
        Just (Cell ( x, y ))

    else
        Nothing


isValidCoordinate : Int -> Bool
isValidCoordinate coordinate =
    coordinate > 0 && coordinate <= rowsAndColumnsAmount


cellBottomLeftCorner : Cell -> LMPoint2d
cellBottomLeftCorner (Cell coordinates) =
    let
        ( cellX, cellY ) =
            coordinates
    in
    if cellX > 0 && cellY > 0 then
        let
            ( xMultiplier, yMultiplier ) =
                ( toFloat (cellX - 1)
                , toFloat (rowsAndColumnsAmount - cellY)
                )
        in
        Point2d.xy
            (tileSize |> Quantity.multiplyBy xMultiplier)
            (tileSize |> Quantity.multiplyBy yMultiplier)

    else
        -- When Cells are built from coordinates, the coordinates are required to be positive.
        -- Invalid input results in a fallback value that is guaranteed to be outside the tilemap.
        Point2d.xy negativeInfinity negativeInfinity


cellCenterPoint : Cell -> LMPoint2d
cellCenterPoint cell =
    let
        displacement =
            Vector2d.xy
                (Quantity.half tileSize)
                (Quantity.half tileSize)
    in
    cellBottomLeftCorner cell
        |> Point2d.translateBy displacement


cellBoundingBox : Cell -> LMBoundingBox2d
cellBoundingBox cell =
    cellBottomLeftCorner cell
        |> Common.boundingBoxWithDimensions tileSize tileSize


cellToString : Cell -> String
cellToString (Cell coordinates) =
    let
        ( x, y ) =
            coordinates
    in
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"


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


nextOrthogonalCell : OrthogonalDirection -> Cell -> Maybe Cell
nextOrthogonalCell dir (Cell coordinates) =
    let
        ( x, y ) =
            coordinates
    in
    case dir of
        Up ->
            cellFromCoordinates ( x, y - 1 )

        Right ->
            cellFromCoordinates ( x + 1, y )

        Down ->
            cellFromCoordinates ( x, y + 1 )

        Left ->
            cellFromCoordinates ( x - 1, y )


nextDiagonalCell : DiagonalDirection -> Cell -> Maybe Cell
nextDiagonalCell dir (Cell coordinates) =
    let
        ( x, y ) =
            coordinates
    in
    case dir of
        TopLeft ->
            cellFromCoordinates ( x - 1, y - 1 )

        TopRight ->
            cellFromCoordinates ( x + 1, y - 1 )

        BottomLeft ->
            cellFromCoordinates ( x - 1, y + 1 )

        BottomRight ->
            cellFromCoordinates ( x + 1, y + 1 )


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


{-| Corner plus natural neighbors (clockwise).

    e.g. Left, TopLeft, Up

-}
cornerCells : DiagonalDirection -> Cell -> List Cell
cornerCells c position =
    case c of
        TopLeft ->
            Maybe.values
                [ nextOrthogonalCell Left position
                , nextDiagonalCell TopLeft position
                , nextOrthogonalCell Up position
                ]

        TopRight ->
            Maybe.values
                [ nextOrthogonalCell Up position
                , nextDiagonalCell TopRight position
                , nextOrthogonalCell Right position
                ]

        BottomLeft ->
            Maybe.values
                [ nextOrthogonalCell Down position
                , nextDiagonalCell BottomLeft position
                , nextOrthogonalCell Left position
                ]

        BottomRight ->
            Maybe.values
                [ nextOrthogonalCell Right position
                , nextDiagonalCell BottomRight position
                , nextOrthogonalCell Down position
                ]



--
-- Tiles
--


emptyTile : Tile
emptyTile =
    -1


horizontalRoad : Tile
horizontalRoad =
    6


verticalRoad : Tile
verticalRoad =
    9


curveBottomRight : Tile
curveBottomRight =
    3


curveBottomLeft : Tile
curveBottomLeft =
    5


curveTopRight : Tile
curveTopRight =
    10


curveTopLeft : Tile
curveTopLeft =
    12


curveTiles : Set Tile
curveTiles =
    Set.fromList
        [ curveTopLeft
        , curveTopRight
        , curveBottomLeft
        , curveBottomRight
        ]


isCurve : Tile -> Bool
isCurve tile =
    Set.member tile curveTiles


deadendDown : Tile
deadendDown =
    1


deadendRight : Tile
deadendRight =
    2


deadendLeft : Tile
deadendLeft =
    4


deadendUp : Tile
deadendUp =
    8


deadendTiles : Set Tile
deadendTiles =
    Set.fromList
        [ deadendDown
        , deadendRight
        , deadendLeft
        , deadendUp
        ]


isDeadend : Tile -> Bool
isDeadend tile =
    Set.member tile deadendTiles


intersectionTUp : Tile
intersectionTUp =
    7


intersectionTLeft : Tile
intersectionTLeft =
    11


intersectionTRight : Tile
intersectionTRight =
    13


intersectionTDown : Tile
intersectionTDown =
    14


intersectionCross : Tile
intersectionCross =
    15


intersectionTiles : Set Tile
intersectionTiles =
    Set.fromList
        [ intersectionTUp
        , intersectionTRight
        , intersectionTDown
        , intersectionTLeft
        , intersectionCross
        ]


isIntersection : Tile -> Bool
isIntersection tile =
    Set.member tile intersectionTiles


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections tile =
    if tile == verticalRoad then
        [ Up, Down ]

    else if tile == horizontalRoad then
        [ Left, Right ]

    else if tile == curveTopRight then
        [ Left, Down ]

    else if tile == curveTopLeft then
        [ Right, Down ]

    else if tile == curveBottomRight then
        [ Left, Up ]

    else if tile == curveBottomLeft then
        [ Right, Up ]

    else if tile == deadendUp then
        [ Up ]

    else if tile == deadendRight then
        [ Right ]

    else if tile == deadendDown then
        [ Down ]

    else if tile == deadendLeft then
        [ Left ]

    else if tile == intersectionTUp then
        Up :: crossOrthogonalDirection Up

    else if tile == intersectionTRight then
        Right :: crossOrthogonalDirection Right

    else if tile == intersectionTDown then
        Down :: crossOrthogonalDirection Down

    else if tile == intersectionTLeft then
        Left :: crossOrthogonalDirection Left

    else if tile == intersectionCross then
        orthogonalDirections

    else
        []
