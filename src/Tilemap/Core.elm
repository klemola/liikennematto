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
    , cellOrthogonalNeighbors
    , createTilemap
    , fixedTileByCell
    , foldTiles
    , forAllTiles
    , getTilemapConfig
    , getTilemapDimensions
    , inTilemapBounds
    , removeAnchor
    , removeTile
    , resetTileBySurroundings
    , setSuperpositionOptions
    , tileByCell
    , tileNeighborIn
    , tileToConfig
    , tilemapBoundingBox
    , tilemapFromCells
    , tilemapIntersects
    , tilemapSize
    , tilemapToList
    , updateTilemap
    )

import Array exposing (Array)
import Array.Extra as Array
import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates, andCarry)
import Data.TileSet as TileSet exposing (tileById, tileIdsFromBitmask)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import Length exposing (Length)
import Lib.Bitmask exposing (OrthogonalNeighbors, fourBitMask)
import Lib.Collection exposing (Id)
import Lib.DiagonalDirection as DiagonalDirection exposing (DiagonalDirection(..))
import Lib.FSM as FSM
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Maybe.Extra as Maybe
import Point2d
import Quantity
import Tilemap.Cell as Cell exposing (Boundary(..), Cell, CellCoordinates)
import Tilemap.Tile as Tile
    exposing
        ( Tile
        , TileKind(..)
        , TileOperation
        )
import Tilemap.TileConfig as TileConfig exposing (TileBiome, TileConfig, TileId)


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


createTilemap : TilemapConfig -> Tilemap
createTilemap tilemapConfig =
    let
        width =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.horizontalCellsAmount)

        height =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.verticalCellsAmount)

        arrSize =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount
    in
    Tilemap
        { cells = Array.initialize arrSize (initTile tilemapConfig)
        , anchors = Dict.empty
        , width = width
        , height = height
        , boundingBox = Common.boundingBoxWithDimensions width height Point2d.origin
        , config = tilemapConfig
        }


tilemapFromCells : TilemapConfig -> List Cell -> Tilemap
tilemapFromCells tilemapConfig cells =
    -- fromCellsHelper cells (createTilemap tilemapConfig)
    -- TODO: Reimplement with WFC
    createTilemap tilemapConfig



-- fromCellsHelper : List Cell -> Tilemap -> Tilemap
-- fromCellsHelper remainingCells tilemap =
--     case remainingCells of
--         [] ->
--             tilemap
--         cell :: others ->
--             let
--                 tilemapUpdateResult =
--                     tilemap
--                         |> applyTilemapOperation Tile.BuildInstantly cell
--                         |> Tuple.first
--                         -- run a FSM update cycle to make sure that tiles are not transitioning
--                         |> updateTilemap (Duration.milliseconds 1000)
--             in
--             fromCellsHelper others tilemapUpdateResult.tilemap


initTile : TilemapConfig -> Int -> Tile
initTile tilemapConfig index =
    index
        |> Cell.fromArray1DIndexUnsafe tilemapConfig
        |> Cell.connectedBounds tilemapConfig
        |> Maybe.map (boundarySuperposition >> Superposition)
        |> Maybe.withDefault (Superposition TileSet.tileIds)
        |> Tile.init


resetTileBySurroundings : Cell -> Tilemap -> Tilemap
resetTileBySurroundings cell tilemap =
    let
        tileIds =
            cellBitmask cell tilemap
                |> tileIdsFromBitmask
    in
    setSuperpositionOptions cell tileIds tilemap


cellBitmask : Cell -> Tilemap -> Int
cellBitmask cell tilemap =
    tilemap
        |> cellOrthogonalNeighbors cell TileConfig.Road
        |> fourBitMask


boundarySuperposition : Cell.Boundary -> List TileId
boundarySuperposition boundary =
    case boundary of
        Corner TopLeft ->
            TileSet.topLeftCornerTileIds

        Corner TopRight ->
            TileSet.topRightCornerTileIds

        Corner BottomLeft ->
            TileSet.bottomLeftCornerTileIds

        Corner BottomRight ->
            TileSet.bottomRightCornerTileIds

        Edge Left ->
            TileSet.leftEdgeTileIds

        Edge Right ->
            TileSet.rightEdgeTileIds

        Edge Up ->
            TileSet.topEdgeTileIds

        Edge Down ->
            TileSet.bottomEdgeTileIds


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

        Superposition _ ->
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
    let
        (Tilemap tilemapContents) =
            tilemap

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
                        Cell.fromArray1DIndex tilemapContents.config index
                            |> Maybe.map
                                (\cell ->
                                    if
                                        (listFilter == StaticTiles)
                                            && (Tile.isDynamic tile || not (Tile.isFixed tile))
                                    then
                                        acc

                                    else
                                        mapperFn cell tile :: acc
                                )
                            |> Maybe.withDefault acc
                    , index = index + 1
                    }
                )
                initialAcc
                tilemapContents.cells
    in
    mappedAcc.acc


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
                        Cell.fromArray1DIndex tilemapContents.config index
                            |> Maybe.map
                                (\cell ->
                                    foldFn cell tile acc
                                )
                            |> Maybe.withDefault acc
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


cellOrthogonalNeighbors : Cell -> TileBiome -> Tilemap -> OrthogonalNeighbors
cellOrthogonalNeighbors origin byBiome tilemap =
    let
        predicate =
            \tileConfig -> TileConfig.biome tileConfig == byBiome
    in
    { up = cellOrthogonalNeighborIn Up origin predicate tilemap
    , left = cellOrthogonalNeighborIn Left origin predicate tilemap
    , right = cellOrthogonalNeighborIn Right origin predicate tilemap
    , down = cellOrthogonalNeighborIn Down origin predicate tilemap
    }


cellOrthogonalNeighborIn : OrthogonalDirection -> Cell -> (TileConfig -> Bool) -> Tilemap -> Bool
cellOrthogonalNeighborIn dir cell tilePredicate tilemap =
    tilemap
        |> tileNeighborIn dir cell
        |> Maybe.andThen (Tuple.second >> Tile.id)
        |> Maybe.map (tileById >> tilePredicate)
        |> Maybe.withDefault False


tileNeighborIn : OrthogonalDirection -> Cell -> Tilemap -> Maybe ( Cell, Tile )
tileNeighborIn dir origin tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap

        maybeCell =
            Cell.nextOrthogonalCell tilemapConfig dir origin

        maybeTile =
            maybeCell
                |> andCarry (fixedTileByCell tilemap)
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

        dynamicCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.dynamicIndices
    in
    { tilemap = Tilemap { currentTilemap | cells = cellsUpdate.nextTiles }
    , actions = cellsUpdate.actions
    , transitionedCells = transitionedCells
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
                Tile.init (Superposition TileSet.tileIds)

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


addTile : Cell -> TileId -> Tilemap -> ( Tilemap, List Tile.Action )
addTile =
    applyTilemapOperation Tile.Add


addTileInstantly : Cell -> TileId -> Tilemap -> ( Tilemap, List Tile.Action )
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


applyTilemapOperation : TileOperation -> Cell -> TileId -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation operation origin tileId tilemap =
    let
        ( originTile, tileActions ) =
            Tile.fromTileId tileId operation
    in
    ( updateCell origin originTile tilemap
    , tileActions
    )


updateCell : Cell -> Tile -> Tilemap -> Tilemap
updateCell cell tile tilemap =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            Cell.array1DIndex tilemapContents.config cell
    in
    Tilemap { tilemapContents | cells = tilemapContents.cells |> Array.set idx tile }


setSuperpositionOptions : Cell -> List TileId -> Tilemap -> Tilemap
setSuperpositionOptions cell nextOptions tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            -- Either retains superposition with next set of tileIds or unfixes the tile, no need to match on the kind
            updateCell cell
                { tile | kind = Superposition nextOptions }
                tilemap

        Nothing ->
            tilemap



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
