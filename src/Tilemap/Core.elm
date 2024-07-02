module Tilemap.Core exposing
    ( TileListFilter(..)
    , Tilemap
    , TilemapConfig
    , TilemapUpdateResult
    , addAnchor
    , addTile
    , addTileInstantly
    , anchorByCell
    , canBuildRoadAt
    , cellBitmask
    , cellHasAnchor
    , cellHasFixedTile
    , createTilemap
    , fixedTileByCell
    , foldTiles
    , forAllTiles
    , getTilemapConfig
    , getTilemapDimensions
    , inTilemapBounds
    , removeAnchor
    , removeTile
    , resetSuperposition
    , resetTileBySurroundings
    , setSuperpositionOptions
    , tileByCell
    , tileNeighborIn
    , tileToConfig
    , tilemapBoundingBox
    , tilemapIntersects
    , tilemapSize
    , tilemapToList
    , updateTilemap
    )

import Array exposing (Array)
import Array.Extra as Array
import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates, andCarry)
import Data.TileSet exposing (tileById, tileIdsByOrthogonalMatch, tileIdsFromBitmask)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import Length exposing (Length)
import Lib.Bitmask exposing (OrthogonalMatch, fourBitMask)
import Lib.Collection exposing (Id)
import Lib.DiagonalDirection as DiagonalDirection exposing (DiagonalDirection(..))
import Lib.FSM as FSM
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Maybe.Extra as Maybe
import Point2d
import Quantity
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Tile as Tile
    exposing
        ( Tile
        , TileKind(..)
        , TileOperation
        )
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId)


type Tilemap
    = Tilemap
        { cells : Array Tile
        , anchors : Dict CellCoordinates ( Id, OrthogonalDirection )
        , width : Length
        , height : Length
        , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
        , config : TilemapConfig
        }


type alias TilemapConfig =
    { horizontalCellsAmount : Int
    , verticalCellsAmount : Int
    }


createTilemap : TilemapConfig -> (Int -> Tile) -> Tilemap
createTilemap tilemapConfig initTileFn =
    let
        width =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.horizontalCellsAmount)

        height =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.verticalCellsAmount)

        arrSize =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount
    in
    Tilemap
        { cells = Array.initialize arrSize initTileFn
        , anchors = Dict.empty
        , width = width
        , height = height
        , boundingBox = Common.boundingBoxWithDimensions width height Point2d.origin
        , config = tilemapConfig
        }


resetTileBySurroundings : Cell -> List TileConfig -> TileKind -> Tilemap -> Tilemap
resetTileBySurroundings cell tileSet tileKind tilemap =
    let
        options =
            case tileKind of
                Fixed _ ->
                    tileIdsFromBitmask (cellBitmask cell tilemap)

                _ ->
                    resetSuperposition cell tileSet tilemap
    in
    setSuperpositionOptions cell options tilemap


resetSuperposition : Cell -> List TileConfig -> Tilemap -> List TileId
resetSuperposition cell tileSet ((Tilemap tilemapContents) as tilemap) =
    Cell.connectedBounds tilemapContents.config cell
        |> Lib.Bitmask.mergeMatches (cellOrthogonalNeighbors cell (\_ -> True) tilemap)
        |> tileIdsByOrthogonalMatch tileSet


cellBitmask : Cell -> Tilemap -> Int
cellBitmask cell tilemap =
    tilemap
        |> cellOrthogonalNeighbors cell cellBitmaskPredicate
        |> fourBitMask


cellBitmaskPredicate : TileConfig -> Bool
cellBitmaskPredicate tileConfig =
    TileConfig.biome tileConfig == TileConfig.Road


fixedTileByCell : Tilemap -> Cell -> Maybe Tile
fixedTileByCell tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            Cell.array1DIndex tilemapContents.config cell
    in
    Array.get idx tilemapContents.cells
        |> Maybe.andThen extractFixedTile


tileByCell : Tilemap -> Cell -> Maybe Tile
tileByCell tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            Cell.array1DIndex tilemapContents.config cell
    in
    Array.get idx tilemapContents.cells


tileToConfig : Tile -> Maybe TileConfig
tileToConfig =
    Tile.id >> Maybe.map tileById


extractFixedTile : Tile -> Maybe Tile
extractFixedTile tile =
    case tile.kind of
        Fixed _ ->
            Just tile

        _ ->
            Nothing


anchorByCell : Tilemap -> Cell -> Maybe ( Id, OrthogonalDirection )
anchorByCell tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap
    in
    Dict.get (Cell.coordinates cell) tilemapContents.anchors


cellHasAnchor : Tilemap -> Cell -> Bool
cellHasAnchor (Tilemap tilemapContents) cell =
    Dict.member (Cell.coordinates cell) tilemapContents.anchors


inTilemapBounds : Tilemap -> BoundingBox2d Length.Meters GlobalCoordinates -> Bool
inTilemapBounds (Tilemap tilemap) testBB =
    BoundingBox2d.isContainedIn tilemap.boundingBox testBB


tilemapIntersects : BoundingBox2d Length.Meters GlobalCoordinates -> Tilemap -> Bool
tilemapIntersects testBB tilemap =
    -- FIXME : upoptimized, skips dynamic tiles
    tilemapToList (\cell _ -> Cell.boundingBox cell) StaticTiles tilemap
        |> List.any (Common.boundingBoxOverlaps testBB)


cellHasFixedTile : Cell -> Tilemap -> Bool
cellHasFixedTile cell tilemap =
    fixedTileByCell tilemap cell
        |> Maybe.andThen extractFixedTile
        |> Maybe.isJust


canBuildRoadAt : Cell -> Tilemap -> Bool
canBuildRoadAt cell tilemap =
    List.all (hasLowComplexity cell tilemap) DiagonalDirection.all


hasLowComplexity : Cell -> Tilemap -> DiagonalDirection -> Bool
hasLowComplexity cell tilemap diagonalDirection =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    Cell.quadrantNeighbors tilemapConfig diagonalDirection cell
        |> List.filterMap (fixedTileByCell tilemap)
        |> (\tiles -> List.length tiles < 3)


type TileListFilter
    = StaticTiles
    | NoFilter


tilemapToList : (Cell -> Tile -> a) -> TileListFilter -> Tilemap -> List a
tilemapToList mapperFn listFilter tilemap =
    foldTiles
        (\cell tile acc ->
            if
                (listFilter == StaticTiles)
                    && (Tile.isDynamic tile || not (Tile.isFixed tile))
            then
                acc

            else
                mapperFn cell tile :: acc
        )
        []
        tilemap


foldTiles : (Cell -> Tile -> b -> b) -> b -> Tilemap -> b
foldTiles foldFn b tilemap =
    let
        (Tilemap tilemapContents) =
            tilemap

        -- Keep track of the array index, which Array.foldr does not
        initialAcc =
            { acc = b
            , index = 0
            }

        mappedAcc =
            -- This is an optimization - Array.indexedMap would require double iteration (cell mapping + Nothing values discarded)
            Array.foldl
                (\tile { acc, index } ->
                    { acc =
                        foldFn
                            (Cell.fromArray1DIndexUnsafe tilemapContents.config index)
                            tile
                            acc
                    , index = index + 1
                    }
                )
                initialAcc
                tilemapContents.cells
    in
    mappedAcc.acc


forAllTiles : (Tile -> Bool) -> Tilemap -> Bool
forAllTiles predicate (Tilemap tilemapContents) =
    -- Room for improvement: early exit if any cell does not satisfy the predicate
    Array.all predicate tilemapContents.cells


getTilemapConfig : Tilemap -> TilemapConfig
getTilemapConfig (Tilemap tilemapContents) =
    tilemapContents.config


tilemapSize : Tilemap -> Int
tilemapSize (Tilemap tilemapContents) =
    Array.foldl
        (\tile count ->
            if Tile.isFixed tile then
                count + 1

            else
                count
        )
        0
        tilemapContents.cells


getTilemapDimensions : Tilemap -> { width : Length, height : Length }
getTilemapDimensions (Tilemap tilemapContents) =
    { width = tilemapContents.width
    , height = tilemapContents.height
    }


tilemapBoundingBox : Tilemap -> BoundingBox2d Length.Meters GlobalCoordinates
tilemapBoundingBox (Tilemap tilemapContents) =
    tilemapContents.boundingBox


cellOrthogonalNeighbors : Cell -> (TileConfig -> Bool) -> Tilemap -> OrthogonalMatch
cellOrthogonalNeighbors origin predicate tilemap =
    { up = cellOrthogonalNeighborIn Up origin predicate tilemap
    , left = cellOrthogonalNeighborIn Left origin predicate tilemap
    , right = cellOrthogonalNeighborIn Right origin predicate tilemap
    , down = cellOrthogonalNeighborIn Down origin predicate tilemap
    }


cellOrthogonalNeighborIn : OrthogonalDirection -> Cell -> (TileConfig -> Bool) -> Tilemap -> Bool
cellOrthogonalNeighborIn dir cell tilePredicate tilemap =
    tilemap
        |> tileNeighborIn dir cell fixedTileByCell
        |> Maybe.andThen (Tuple.second >> Tile.id)
        |> Maybe.map (tileById >> tilePredicate)
        |> Maybe.withDefault False


tileNeighborIn : OrthogonalDirection -> Cell -> (Tilemap -> Cell -> Maybe Tile) -> Tilemap -> Maybe ( Cell, Tile )
tileNeighborIn dir origin getTileFn tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap

        maybeCell =
            Cell.nextOrthogonalCell tilemapConfig dir origin

        maybeTile =
            maybeCell
                |> andCarry (getTileFn tilemap)
                |> Maybe.andThen
                    (\( neighborCell, tile ) ->
                        let
                            state =
                                FSM.toCurrentState tile.fsm
                        in
                        -- tiles pending removal should not be used when checking tile neighbors
                        if state == Tile.Removing || state == Tile.Removed then
                            Nothing

                        else
                            Just ( neighborCell, tile )
                    )
    in
    maybeTile



--
-- Update
--


type alias TilemapUpdate =
    { nextTiles : Array Tile
    , actions : List Tile.Action

    -- Room for improvement: keep a single list with an union that describes the indices' status
    , emptiedIndices : List Int
    , transitionedIndices : List Int
    , dynamicIndices : List Int
    }


type alias TilemapUpdateResult =
    { tilemap : Tilemap
    , actions : List Tile.Action
    , transitionedCells : List Cell
    , emptiedCells : List Cell
    , dynamicCells : List Cell
    }


updateTilemap : Duration -> Tilemap -> TilemapUpdateResult
updateTilemap delta tilemap =
    let
        (Tilemap currentTilemap) =
            tilemap

        cellsUpdate =
            Array.foldl
                (updateTile delta)
                { nextTiles = Array.empty
                , actions = []
                , emptiedIndices = []
                , transitionedIndices = []
                , dynamicIndices = []
                }
                currentTilemap.cells

        transitionedCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.transitionedIndices

        emptiedCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.emptiedIndices

        dynamicCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.dynamicIndices
    in
    { tilemap = Tilemap { currentTilemap | cells = cellsUpdate.nextTiles }
    , actions = cellsUpdate.actions
    , transitionedCells = transitionedCells
    , emptiedCells = emptiedCells
    , dynamicCells = dynamicCells
    }


updateTile : Duration -> Tile -> TilemapUpdate -> TilemapUpdate
updateTile delta tile tilemapUpdate =
    let
        idx =
            Array.length tilemapUpdate.nextTiles

        ( nextFSM, tileActions ) =
            FSM.updateWithoutContext delta tile.fsm

        isRemoved =
            FSM.toCurrentState nextFSM == Tile.Removed

        nextTile =
            if isRemoved then
                Tile.init Unintialized

            else
                { kind = tile.kind
                , fsm = nextFSM
                }

        nextEmptiedIndices =
            if isRemoved then
                idx :: tilemapUpdate.emptiedIndices

            else
                tilemapUpdate.emptiedIndices

        nextTransitionedIndices =
            if FSM.toCurrentState tile.fsm /= FSM.toCurrentState nextFSM then
                idx :: tilemapUpdate.transitionedIndices

            else
                tilemapUpdate.transitionedIndices

        nextDynamicIndices =
            if Tile.isDynamic nextTile then
                idx :: tilemapUpdate.dynamicIndices

            else
                tilemapUpdate.dynamicIndices
    in
    { nextTiles = tilemapUpdate.nextTiles |> Array.push nextTile
    , actions = tilemapUpdate.actions ++ tileActions
    , emptiedIndices = nextEmptiedIndices
    , transitionedIndices = nextTransitionedIndices
    , dynamicIndices = nextDynamicIndices
    }


addTile : TileId -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTile =
    applyTilemapOperation Tile.Add


addTileInstantly : TileId -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTileInstantly =
    applyTilemapOperation Tile.BuildInstantly


removeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
removeTile origin tilemap =
    case
        fixedTileByCell tilemap origin
            |> Maybe.map Tile.attemptRemove
    of
        Just ( tile, actions ) ->
            ( updateCell origin tile tilemap
            , actions
            )

        Nothing ->
            ( tilemap, [] )


applyTilemapOperation : TileOperation -> TileId -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation operation tileId origin tilemap =
    let
        ( originTile, tileActions ) =
            Tile.fromTileId tileId operation
    in
    ( updateCell origin originTile tilemap
    , tileActions
    )


updateCell : Cell -> Tile -> Tilemap -> Tilemap
updateCell cell tile (Tilemap tilemapContents) =
    let
        idx =
            Cell.array1DIndex tilemapContents.config cell
    in
    Tilemap { tilemapContents | cells = tilemapContents.cells |> Array.set idx tile }


setSuperpositionOptions : Cell -> List TileId -> Tilemap -> Tilemap
setSuperpositionOptions cell nextOptions tilemap =
    let
        updatedTile =
            case tileByCell tilemap cell of
                Just tile ->
                    { tile | kind = Superposition nextOptions }

                Nothing ->
                    Tile.init (Superposition nextOptions)
    in
    -- Either retains superposition with next set of tileIds or unfixes/intializes the tile, no need to match on the tile kind
    updateCell cell updatedTile tilemap



--
-- Anchors
--


addAnchor : Cell -> Id -> OrthogonalDirection -> Tilemap -> Tilemap
addAnchor anchor lotId anchorDirection (Tilemap tilemapContents) =
    let
        nextAnchors =
            Dict.insert
                (Cell.coordinates anchor)
                ( lotId, anchorDirection )
                tilemapContents.anchors
    in
    Tilemap { tilemapContents | anchors = nextAnchors }


removeAnchor : Id -> Tilemap -> Tilemap
removeAnchor lotId tilemap =
    let
        (Tilemap tilemapContents) =
            tilemap

        anchor =
            Dict.find
                (\_ ( anchorLotId, _ ) -> anchorLotId == lotId)
                tilemapContents.anchors
    in
    case anchor |> Maybe.andThen (anchorCell tilemap) of
        Just cell ->
            let
                cellCoordinates =
                    Cell.coordinates cell
            in
            Tilemap { tilemapContents | anchors = Dict.remove cellCoordinates tilemapContents.anchors }

        Nothing ->
            tilemap


anchorCell : Tilemap -> ( CellCoordinates, ( Id, OrthogonalDirection ) ) -> Maybe Cell
anchorCell tilemap ( cellCoordinates, _ ) =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    Cell.fromCoordinates tilemapConfig cellCoordinates
