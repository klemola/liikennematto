module Model.Tilemap exposing
    ( Cell
    , Tilemap
    , addTile
    , boundingBox
    , canBuildRoadAt
    , cellBottomLeftCorner
    , cellBoundingBox
    , cellCenterPoint
    , cellFromCoordinates
    , cellToString
    , empty
    , exists
    , inBounds
    , intersects
    , mapSize
    , nextOrthogonalCell
    , removeTile
    , rowsAndColumnsAmount
    , size
    , tileAt
    , toList
    , update
    )

import Array exposing (Array)
import BoundingBox2d
import Common
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        )
import Model.OrthogonalDirection
    exposing
        ( DiagonalDirection(..)
        , OrthogonalDirection(..)
        , diagonalDirections
        )
import Model.Tile as Tile
    exposing
        ( Tile
        , TileKind
        , TileOperation
        , chooseTileKind
        , tileSize
        )
import Point2d
import Quantity exposing (negativeInfinity)
import Vector2d


type Tilemap
    = Tilemap (Array (Maybe Tile))


type Cell
    = Cell CellCoordinates


type alias CellCoordinates =
    ( Int, Int )



--
-- Tilemap
--


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
    Tilemap (Array.initialize arrSize (always Nothing))


tileAt : Tilemap -> Cell -> Maybe Tile
tileAt (Tilemap tilemapContents) cell =
    let
        idx =
            indexFromCell cell
    in
    Array.get idx tilemapContents
        |> Maybe.andThen identity


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


canBuildRoadAt : Cell -> Tilemap -> Bool
canBuildRoadAt cell tilemap =
    let
        withinAllowedComplexity l =
            List.length l < 3

        hasLowComplexity diagonalDirection =
            cornerCells diagonalDirection cell
                |> List.filterMap (tileAt tilemap)
                |> withinAllowedComplexity
    in
    List.all hasLowComplexity diagonalDirections


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
                (\maybeTile { acc, index } ->
                    { acc =
                        case maybeTile of
                            Just tile ->
                                cellFromIndex index
                                    |> Maybe.map (\cell -> mapperFn cell tile :: acc)
                                    |> Maybe.withDefault acc

                            Nothing ->
                                acc
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
        (\maybeTile count ->
            case maybeTile of
                Just _ ->
                    count + 1

                Nothing ->
                    count
        )
        0
        tilemapContents



--
-- Update
--


type alias TilemapUpdate =
    { tilemap : Array (Maybe Tile)
    , actions : List Tile.Action
    , emptiedIndices : List Int
    , changedIndices : List Int
    }


update : Duration -> Tilemap -> ( Tilemap, List Tile.Action, List Cell )
update delta tilemap =
    let
        (Tilemap currentTilemap) =
            tilemap

        tilemapUpdate =
            Array.foldl
                (maybeUpdateTile delta)
                { tilemap = Array.empty
                , actions = []
                , emptiedIndices = []
                , changedIndices = []
                }
                currentTilemap

        nextTilemap =
            List.foldl
                (\idx acc ->
                    let
                        maybeCell =
                            cellFromIndex idx
                    in
                    case maybeCell of
                        Just cell ->
                            updateNeighborCells cell acc

                        Nothing ->
                            acc
                )
                (Tilemap tilemapUpdate.tilemap)
                tilemapUpdate.emptiedIndices

        changedCells =
            tilemapUpdate.changedIndices
                |> List.map cellFromIndex
                |> Maybe.values
    in
    ( nextTilemap, tilemapUpdate.actions, changedCells )


maybeUpdateTile : Duration -> Maybe Tile -> TilemapUpdate -> TilemapUpdate
maybeUpdateTile delta maybeTile tilemapUpdate =
    case maybeTile of
        Just tile ->
            updateTileFSM delta tile tilemapUpdate

        Nothing ->
            { tilemap = tilemapUpdate.tilemap |> Array.push Nothing
            , actions = tilemapUpdate.actions
            , emptiedIndices = tilemapUpdate.emptiedIndices
            , changedIndices = tilemapUpdate.changedIndices
            }


updateTileFSM : Duration -> Tile -> TilemapUpdate -> TilemapUpdate
updateTileFSM delta tile tilemapUpdate =
    let
        idx =
            Array.length tilemapUpdate.tilemap

        ( nextFSM, tileActions ) =
            FSM.update delta tile.fsm

        isRemoved =
            FSM.toCurrentState nextFSM == Tile.Removed

        nextTile =
            if isRemoved then
                Nothing

            else
                Just
                    { kind = tile.kind
                    , fsm = nextFSM
                    }

        nextEmptiedIndices =
            if isRemoved then
                idx :: tilemapUpdate.emptiedIndices

            else
                tilemapUpdate.emptiedIndices

        nextChangedIndices =
            if FSM.toCurrentState tile.fsm /= FSM.toCurrentState nextFSM then
                idx :: tilemapUpdate.changedIndices

            else
                tilemapUpdate.changedIndices
    in
    { tilemap = tilemapUpdate.tilemap |> Array.push nextTile
    , actions = tilemapUpdate.actions ++ tileActions
    , emptiedIndices = nextEmptiedIndices
    , changedIndices = nextChangedIndices
    }


addTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTile cell tilemap =
    applyTilemapOperation cell Tile.Add tilemap


removeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
removeTile cell tilemap =
    applyTilemapOperation cell Tile.Remove tilemap


applyTilemapOperation : Cell -> TileOperation -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation origin tileChange tilemap =
    let
        originTileKind =
            chooseTile tilemap origin

        ( originTile, initialActions ) =
            Tile.new originTileKind tileChange

        tilemapWithOriginChange =
            updateCell origin originTile tilemap
    in
    ( updateNeighborCells origin tilemapWithOriginChange
    , initialActions
    )


updateNeighborCells : Cell -> Tilemap -> Tilemap
updateNeighborCells origin tilemap =
    let
        potentiallyChangedCells =
            [ nextOrthogonalTile Up origin tilemap
            , nextOrthogonalTile Left origin tilemap
            , nextOrthogonalTile Right origin tilemap
            , nextOrthogonalTile Down origin tilemap
            ]
    in
    potentiallyChangedCells
        |> Maybe.values
        |> List.foldl
            (\( changedCell, _ ) acc ->
                let
                    nextTileKind =
                        chooseTile acc changedCell

                    -- "reset" the tile & FSM to avoid glitchy transitions
                    -- FSM actions are ignored
                    ( nextTile, _ ) =
                        Tile.new nextTileKind Tile.Change
                in
                updateCell changedCell nextTile acc
            )
            tilemap


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


chooseTile : Tilemap -> Cell -> TileKind
chooseTile tilemap origin =
    let
        orthogonalNeighbors =
            { up = hasOrthogonalNeighborAt Up origin tilemap
            , left = hasOrthogonalNeighborAt Left origin tilemap
            , right = hasOrthogonalNeighborAt Right origin tilemap
            , down = hasOrthogonalNeighborAt Down origin tilemap
            }
    in
    chooseTileKind orthogonalNeighbors


hasOrthogonalNeighborAt : OrthogonalDirection -> Cell -> Tilemap -> Bool
hasOrthogonalNeighborAt dir cell tilemap =
    nextOrthogonalCell dir cell |> Maybe.unwrap False (\neighborCell -> exists neighborCell tilemap)


updateCell : Cell -> Tile -> Tilemap -> Tilemap
updateCell cell tile (Tilemap tilemapContents) =
    let
        idx =
            indexFromCell cell

        tiles =
            tilemapContents |> Array.set idx (Just tile)
    in
    Tilemap tiles



--
-- Cells
--


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
