module Model.Tilemap exposing
    ( Tilemap
    , addAnchor
    , addTile
    , anchorAt
    , boundingBox
    , canBuildRoadAt
    , empty
    , exists
    , fromCells
    , hasAnchor
    , inBounds
    , intersects
    , mapSize
    , removeAnchor
    , removeTile
    , size
    , tileAt
    , toList
    , update
    )

import Array exposing (Array)
import BoundingBox2d
import Common
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( DiagonalDirection(..)
        , LMBoundingBox2d
        , OrthogonalDirection(..)
        , diagonalDirections
        )
import Model.Tile as Tile
    exposing
        ( Tile
        , TileKind
        , TileOperation
        , chooseTileKind
        )
import Point2d
import Quantity


type Tilemap
    = Tilemap
        { cells : Array (Maybe Tile)
        , anchors : Dict CellCoordinates ( Id, OrthogonalDirection )
        }


mapSize : Length
mapSize =
    -- TODO: check usage and consider tuple (sizeX, sizeY)
    Cell.size |> Quantity.multiplyBy (toFloat Cell.horizontalCellsAmount)


boundingBox : LMBoundingBox2d
boundingBox =
    Common.boundingBoxWithDimensions mapSize mapSize Point2d.origin


empty : Tilemap
empty =
    let
        arrSize =
            Cell.horizontalCellsAmount * Cell.verticalCellsAmount
    in
    Tilemap
        { cells = Array.initialize arrSize (always Nothing)
        , anchors = Dict.empty
        }


fromCells : List Cell -> Tilemap
fromCells cells =
    fromCellsHelper cells empty


fromCellsHelper : List Cell -> Tilemap -> Tilemap
fromCellsHelper remainingCells tilemap =
    case remainingCells of
        [] ->
            tilemap

        cell :: others ->
            let
                tilemapUpdateResult =
                    tilemap
                        |> applyTilemapOperation cell Tile.BuildInstantly
                        |> Tuple.first
                        -- run a FSM update cycle to make sure that tiles are not transitioning
                        |> update (Duration.milliseconds 1000)
            in
            fromCellsHelper others tilemapUpdateResult.tilemap


tileAt : Tilemap -> Cell -> Maybe Tile
tileAt (Tilemap tilemapContents) cell =
    let
        idx =
            indexFromCell cell
    in
    Array.get idx tilemapContents.cells
        |> Maybe.andThen identity


anchorAt : Tilemap -> Cell -> Maybe ( Id, OrthogonalDirection, Tile )
anchorAt tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap

        anchor =
            Dict.get (Cell.coordinates cell) tilemapContents.anchors

        tile =
            tileAt tilemap cell
    in
    Maybe.map2
        (\( id, anchorDir ) anchorTile -> ( id, anchorDir, anchorTile ))
        anchor
        tile


hasAnchor : Tilemap -> Cell -> Bool
hasAnchor (Tilemap tilemapContents) cell =
    Dict.member (Cell.coordinates cell) tilemapContents.anchors


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


intersects : LMBoundingBox2d -> Tilemap -> Bool
intersects testBB tilemap =
    toList (\cell _ -> Cell.boundingBox cell) tilemap
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
            Cell.quadrantNeighbors diagonalDirection cell
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
                tilemapContents.cells
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
        tilemapContents.cells


updateCell : Cell -> Tile -> Tilemap -> Tilemap
updateCell cell tile (Tilemap tilemapContents) =
    let
        idx =
            indexFromCell cell
    in
    Tilemap
        { cells = tilemapContents.cells |> Array.set idx (Just tile)
        , anchors = tilemapContents.anchors
        }


cellFromIndex : Int -> Maybe Cell
cellFromIndex idx =
    let
        xyZeroIndexed =
            { x = remainderBy Cell.horizontalCellsAmount idx
            , y = idx // Cell.verticalCellsAmount
            }
    in
    -- Cells are 1-indexed - map the coordinates to match
    Cell.fromCoordinates
        ( xyZeroIndexed.x + 1
        , xyZeroIndexed.y + 1
        )


indexFromCell : Cell -> Int
indexFromCell cell =
    let
        ( cellX, cellY ) =
            Cell.coordinates cell

        xyZeroIndexed =
            { x = cellX - 1
            , y = cellY - 1
            }
    in
    -- Arrays are 0-indexed - map the coordinates to match
    xyZeroIndexed.x + (xyZeroIndexed.y * Cell.verticalCellsAmount)



--
-- Update
--


type alias TilemapUpdate =
    { nextCells : Array (Maybe Tile)
    , actions : List Tile.Action
    , emptiedIndices : List Int
    , changedIndices : List Int
    }


type alias TilemapUpdateResult =
    { tilemap : Tilemap
    , actions : List Tile.Action
    , changedCells : List Cell
    }


update : Duration -> Tilemap -> TilemapUpdateResult
update delta tilemap =
    let
        (Tilemap currentTilemap) =
            tilemap

        cellsUpdate =
            Array.foldl
                (maybeUpdateTile delta)
                { nextCells = Array.empty
                , actions = []
                , emptiedIndices = []
                , changedIndices = []
                }
                currentTilemap.cells

        nextTilemap =
            List.foldl
                (\idx acc ->
                    case cellFromIndex idx of
                        Just cell ->
                            updateNeighborCells cell acc

                        Nothing ->
                            acc
                )
                (Tilemap
                    { cells = cellsUpdate.nextCells
                    , anchors = currentTilemap.anchors
                    }
                )
                cellsUpdate.emptiedIndices

        changedCells =
            cellsUpdate.changedIndices
                |> List.map cellFromIndex
                |> Maybe.values
    in
    { tilemap = nextTilemap
    , actions = cellsUpdate.actions
    , changedCells = changedCells
    }


maybeUpdateTile : Duration -> Maybe Tile -> TilemapUpdate -> TilemapUpdate
maybeUpdateTile delta maybeTile tilemapUpdate =
    case maybeTile of
        Just tile ->
            updateTileFSM delta tile tilemapUpdate

        Nothing ->
            { nextCells = tilemapUpdate.nextCells |> Array.push Nothing
            , actions = tilemapUpdate.actions
            , emptiedIndices = tilemapUpdate.emptiedIndices
            , changedIndices = tilemapUpdate.changedIndices
            }


updateTileFSM : Duration -> Tile -> TilemapUpdate -> TilemapUpdate
updateTileFSM delta tile tilemapUpdate =
    let
        idx =
            Array.length tilemapUpdate.nextCells

        ( nextFSM, tileActions ) =
            FSM.updateWithoutContext delta tile.fsm

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
    { nextCells = tilemapUpdate.nextCells |> Array.push nextTile
    , actions = tilemapUpdate.actions ++ tileActions
    , emptiedIndices = nextEmptiedIndices
    , changedIndices = nextChangedIndices
    }


addTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTile origin tilemap =
    applyTilemapOperation origin Tile.Add tilemap


removeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
removeTile origin tilemap =
    case
        tileAt tilemap origin
            |> Maybe.map Tile.attemptRemove
    of
        Just ( tile, actions ) ->
            let
                tilemapWithOriginChange =
                    updateCell origin tile tilemap
            in
            ( updateNeighborCells origin tilemapWithOriginChange
            , actions
            )

        Nothing ->
            ( tilemap, [] )


setAnchorTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
setAnchorTile anchor tilemap =
    case tileAt tilemap anchor of
        Just tile ->
            let
                anchorTileKind =
                    chooseTile tilemap anchor

                ( anchorTile, actions ) =
                    Tile.updateTileKind anchorTileKind tile
            in
            ( updateCell anchor anchorTile tilemap, actions )

        Nothing ->
            ( tilemap, [] )


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
            (\( changedCell, tile ) acc ->
                let
                    nextTileKind =
                        chooseTile acc changedCell

                    -- FSM actions are ignored
                    ( nextTile, _ ) =
                        Tile.updateTileKind nextTileKind tile
                in
                updateCell changedCell nextTile acc
            )
            tilemap


addAnchor : Cell -> Id -> OrthogonalDirection -> Tilemap -> Tilemap
addAnchor anchor lotId anchorDirection (Tilemap tilemapContents) =
    let
        nextAnchors =
            Dict.insert
                (Cell.coordinates anchor)
                ( lotId, anchorDirection )
                tilemapContents.anchors

        tilemapWithAnchor =
            Tilemap
                { anchors = nextAnchors
                , cells = tilemapContents.cells
                }
    in
    setAnchorTile anchor tilemapWithAnchor |> Tuple.first


removeAnchor : Id -> Tilemap -> Tilemap
removeAnchor lotId (Tilemap tilemapContents) =
    Tilemap
        { anchors =
            Dict.filter
                (\_ ( anchorLotId, _ ) -> anchorLotId /= lotId)
                tilemapContents.anchors
        , cells = tilemapContents.cells
        }


nextOrthogonalTile : OrthogonalDirection -> Cell -> Tilemap -> Maybe ( Cell, Tile )
nextOrthogonalTile dir cell tilemap =
    let
        maybeCell =
            Cell.nextOrthogonalCell dir cell

        maybeTile =
            maybeCell
                |> Maybe.andThen (tileAt tilemap)
                |> Maybe.andThen
                    (\tile ->
                        let
                            state =
                                FSM.toCurrentState tile.fsm
                        in
                        -- tiles pending removal should not be used when checking tile neighbors
                        if state == Tile.Removing || state == Tile.Removed then
                            Nothing

                        else
                            Just tile
                    )
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

        lotModifier =
            hasAnchor tilemap origin
    in
    chooseTileKind orthogonalNeighbors lotModifier


hasOrthogonalNeighborAt : OrthogonalDirection -> Cell -> Tilemap -> Bool
hasOrthogonalNeighborAt dir cell tilemap =
    hasNeighborTileAt dir cell tilemap || hasLotAt dir cell tilemap


hasNeighborTileAt : OrthogonalDirection -> Cell -> Tilemap -> Bool
hasNeighborTileAt dir cell tilemap =
    tilemap
        |> nextOrthogonalTile dir cell
        |> Maybe.isJust


hasLotAt : OrthogonalDirection -> Cell -> Tilemap -> Bool
hasLotAt anchorDir anchor tilemap =
    case anchorAt tilemap anchor of
        Just ( _, dir, _ ) ->
            dir == anchorDir

        Nothing ->
            False
