module Tilemap.Core exposing
    ( TileListFilter(..)
    , Tilemap
    , TilemapConfig
    , TilemapUpdateResult
    , addAnchor
    , addAnimationTimer
    , addTile
    , addTileFromWfc
    , anchorByCell
    , cellBitmask
    , cellSupportsRoadPlacement
    , clearTile
    , createTilemap
    , extractRoadTile
    , fixedTileByCell
    , foldTiles
    , getBuildHistory
    , getTilemapConfig
    , getTilemapDimensions
    , mapCell
    , removeAnchor
    , removeTile
    , resetFixedTileBySurroundings
    , resetSuperposition
    , resetTileBySurroundings
    , roadTileFromCell
    , setBuildHistory
    , setSuperpositionOptions
    , tileByCell
    , tileNeighborIn
    , tileToConfig
    , tilemapBoundingBox
    , tilemapToList
    , updateTilemap
    )

import Array exposing (Array)
import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates, andCarry)
import Data.TileSet
    exposing
        ( decorativeTiles
        , extractLotEntryTile
        , lotDrivewaySocket
        , lotEntrySocket
        , tileById
        , tileIdsByOrthogonalMatch
        , tileIdsFromBitmask
        )
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import Length exposing (Length)
import Lib.Bitmask exposing (OrthogonalMatch, fourBitMask)
import Lib.Collection exposing (Id)
import Lib.DiagonalDirection as DiagonalDirection exposing (DiagonalDirection)
import Lib.FSM as FSM
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Maybe.Extra as Maybe
import Point2d
import Quantity
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates, nextOrthogonalCell)
import Tilemap.Tile as Tile
    exposing
        ( Tile
        , TileKind(..)
        , TileOperation
        )
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId, directionBySocket)


type Tilemap
    = Tilemap
        { cells : Array Tile
        , anchors : Dict CellCoordinates ( Id, OrthogonalDirection )
        , width : Length
        , height : Length
        , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
        , config : TilemapConfig
        , recentPlacements : List Cell
        , animationTimers : List ( Cell, Duration )
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
        , recentPlacements = []
        , animationTimers = []
        }


resetTileBySurroundings : Cell -> List TileConfig -> Tilemap -> Tilemap
resetTileBySurroundings cell tileSet tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            case tile.kind of
                Fixed _ ->
                    tilemap

                _ ->
                    setSuperpositionOptions cell
                        (resetSuperposition cell tileSet tilemap)
                        tilemap

        Nothing ->
            tilemap


resetFixedTileBySurroundings : Cell -> Tilemap -> Tilemap
resetFixedTileBySurroundings cell tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            case tile.kind of
                Fixed current ->
                    let
                        base =
                            setSuperpositionOptions
                                cell
                                (tileIdsFromBitmask (cellBitmask cell tilemap))
                                tilemap

                        currentTileConfig =
                            tileById current.id
                    in
                    directionBySocket (TileConfig.sockets currentTileConfig) lotEntrySocket
                        |> Maybe.andThen
                            (\lotEntryDirection ->
                                tileNeighborIn lotEntryDirection cell fixedTileByCell tilemap
                            )
                        |> Maybe.andThen
                            (\( subgridAnchorCell, subgridAnchorTile ) ->
                                Maybe.map
                                    (\parentTile ->
                                        removeLargeTileSubtiles subgridAnchorCell parentTile base
                                    )
                                    (tileParentTile subgridAnchorTile)
                            )
                        |> Maybe.withDefault base

                _ ->
                    tilemap

        Nothing ->
            tilemap


resetSuperposition : Cell -> List TileConfig -> Tilemap -> List TileId
resetSuperposition cell tileSet ((Tilemap tilemapContents) as tilemap) =
    let
        filterNeighbor neighborTile =
            case neighborTile.kind of
                Fixed _ ->
                    True

                _ ->
                    False
    in
    Cell.connectedBounds tilemapContents.config cell
        |> Lib.Bitmask.mergeMatches (cellOrthogonalNeighbors cell filterNeighbor tilemap)
        |> tileIdsByOrthogonalMatch tileSet


cellBitmask : Cell -> Tilemap -> Int
cellBitmask cell tilemap =
    tilemap
        |> cellOrthogonalNeighbors cell (extractRoadTile >> Maybe.isJust)
        |> fourBitMask


fixedTileByCell : Tilemap -> Cell -> Maybe Tile
fixedTileByCell tilemap cell =
    tileByCell tilemap cell
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


setBuildHistory : List Cell -> Tilemap -> Tilemap
setBuildHistory history (Tilemap tilemapContents) =
    Tilemap { tilemapContents | recentPlacements = history }


getBuildHistory : Tilemap -> List Cell
getBuildHistory (Tilemap tilemapContents) =
    tilemapContents.recentPlacements


addAnimationTimer : Cell -> Duration -> Tilemap -> Tilemap
addAnimationTimer cell timer (Tilemap tilemapContents) =
    Tilemap { tilemapContents | animationTimers = ( cell, timer ) :: tilemapContents.animationTimers }


cellSupportsRoadPlacement : Cell -> Tilemap -> Bool
cellSupportsRoadPlacement cell tilemap =
    List.all (cellHasLowComplexity cell tilemap) DiagonalDirection.all


roadTileFromCell : Cell -> Tilemap -> Maybe Tile
roadTileFromCell cell tilemap =
    tileByCell tilemap cell
        |> Maybe.andThen extractRoadTile


extractRoadTile : Tile -> Maybe Tile
extractRoadTile tile =
    Tile.id tile
        |> Maybe.map tileById
        |> Maybe.andThen
            (\tileConfig ->
                if TileConfig.biome tileConfig == TileConfig.Road then
                    Just tile

                else
                    Nothing
            )


cellHasLowComplexity : Cell -> Tilemap -> DiagonalDirection -> Bool
cellHasLowComplexity cell tilemap diagonalDirection =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    Cell.quadrantNeighbors tilemapConfig diagonalDirection cell
        |> List.filterMap
            (\quadrantNeighbor ->
                roadTileFromCell quadrantNeighbor tilemap |> Maybe.map (\_ -> quadrantNeighbor)
            )
        |> (\tiles -> List.length tiles < 3)


type TileListFilter
    = StaticTiles
    | NoFilter


tilemapToList : (Cell -> Tile -> Maybe a) -> TileListFilter -> Tilemap -> List a
tilemapToList mapperFn listFilter tilemap =
    foldTiles
        (\cell tile acc ->
            if
                (listFilter == StaticTiles)
                    && (Tile.isDynamic tile || not (Tile.isFixed tile))
            then
                acc

            else
                case mapperFn cell tile of
                    Just v ->
                        v :: acc

                    Nothing ->
                        acc
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


getTilemapConfig : Tilemap -> TilemapConfig
getTilemapConfig (Tilemap tilemapContents) =
    tilemapContents.config


getTilemapDimensions : Tilemap -> { width : Length, height : Length }
getTilemapDimensions (Tilemap tilemapContents) =
    { width = tilemapContents.width
    , height = tilemapContents.height
    }


tilemapBoundingBox : Tilemap -> BoundingBox2d Length.Meters GlobalCoordinates
tilemapBoundingBox (Tilemap tilemapContents) =
    tilemapContents.boundingBox


cellOrthogonalNeighbors : Cell -> (Tile -> Bool) -> Tilemap -> OrthogonalMatch
cellOrthogonalNeighbors origin predicate tilemap =
    let
        neighborIn dir =
            tileNeighborIn dir origin tileByCell tilemap
                |> Maybe.map (Tuple.second >> predicate)
                |> Maybe.withDefault False
    in
    { up = neighborIn Up
    , left = neighborIn Left
    , right = neighborIn Right
    , down = neighborIn Down
    }


tileNeighborIn : OrthogonalDirection -> Cell -> (Tilemap -> Cell -> Maybe Tile) -> Tilemap -> Maybe ( Cell, Tile )
tileNeighborIn dir origin getTileFn tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap

        maybeCell =
            Cell.nextOrthogonalCell tilemapConfig dir origin
    in
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

        ( cellsWithFinishedTimers, nextTimers ) =
            List.foldl
                (\( timerCell, timer ) ( finishedAcc, timersAcc ) ->
                    let
                        nextTimer =
                            timer
                                |> Quantity.minus delta
                                |> Quantity.max Quantity.zero
                    in
                    if Quantity.lessThanOrEqualToZero nextTimer then
                        ( timerCell :: finishedAcc, timersAcc )

                    else
                        ( finishedAcc, ( timerCell, nextTimer ) :: timersAcc )
                )
                ( [], [] )
                currentTilemap.animationTimers
    in
    { tilemap =
        List.foldl
            (\cell updatedTilemap ->
                mapCell cell (Tile.withAnimation Nothing) updatedTilemap
            )
            (Tilemap
                { currentTilemap
                    | cells = cellsUpdate.nextTiles
                    , animationTimers = nextTimers
                }
            )
            cellsWithFinishedTimers
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


addTile : TileConfig -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTile =
    applyTilemapOperation Tile.Add Nothing


addTileFromWfc : Maybe ( TileId, Int ) -> TileConfig -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTileFromWfc parentTile =
    applyTilemapOperation Tile.AddFromWFC parentTile


removeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
removeTile origin ((Tilemap tilemapContents) as tilemap) =
    case
        fixedTileByCell tilemap origin
            |> Maybe.map Tile.attemptRemove
    of
        Just ( tile, actions ) ->
            case tileParentTile tile of
                Just parentTile ->
                    ( tilemap
                        |> removeLargeTileSubtiles origin parentTile
                        |> resetLargeTileAnchor origin parentTile
                    , actions
                    )

                Nothing ->
                    let
                        withUpdatedCell =
                            updateCell origin tile tilemap

                        ( withMaybeConnectedLargeTileRemoved, removeLargeTileActions ) =
                            Tile.id tile
                                |> Maybe.andThen extractLotEntryTile
                                |> Maybe.andThen
                                    (\( _, directionToLot ) ->
                                        nextOrthogonalCell tilemapContents.config directionToLot origin
                                    )
                                |> Maybe.map
                                    (\lotDrivewayCell ->
                                        -- Call removeTile in a recursive way, as this will branch out to the "parent tile" found
                                        -- case or at least fail on the neighbor not being a lot entry cell
                                        removeTile lotDrivewayCell withUpdatedCell
                                    )
                                |> Maybe.withDefault ( withUpdatedCell, [] )
                    in
                    ( withMaybeConnectedLargeTileRemoved
                    , actions ++ removeLargeTileActions
                    )

        Nothing ->
            ( tilemap, [] )


clearTile : Cell -> Tilemap -> Tilemap
clearTile cell tilemap =
    updateCell cell (Tile.init Tile.Unintialized) tilemap


applyTilemapOperation : TileOperation -> Maybe ( TileId, Int ) -> TileConfig -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation operation parentTileId tileConfig origin tilemap =
    let
        ( originTile, tileActions ) =
            Tile.fromTileConfig tileConfig parentTileId operation
    in
    ( updateCell origin
        originTile
        (case Tile.animation originTile of
            Just animation ->
                addAnimationTimer origin animation.duration tilemap

            Nothing ->
                tilemap
        )
    , tileActions
    )


mapCell : Cell -> (Tile -> Tile) -> Tilemap -> Tilemap
mapCell cell tileFn tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            updateCell cell (tileFn tile) tilemap

        Nothing ->
            tilemap


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
            Tile.init (Superposition nextOptions)
    in
    -- Either retains superposition with next set of tileIds or unfixes/intializes the tile, no need to match on the tile kind
    updateCell cell updatedTile tilemap



--
-- Large tiles, subgrids
--


tileParentTile : Tile -> Maybe ( TileId, Int )
tileParentTile tile =
    case tile.kind of
        Fixed properties ->
            properties.parentTile

        _ ->
            Nothing


removeLargeTileSubtiles : Cell -> ( TileId, Int ) -> Tilemap -> Tilemap
removeLargeTileSubtiles subtileCell ( largeTileId, subtileIndex ) ((Tilemap tilemapContents) as tilemap) =
    case tileById largeTileId of
        TileConfig.Large largeTile ->
            case
                Tile.largeTileTopLeftCell
                    tilemapContents.config
                    subtileCell
                    subtileIndex
                    largeTile
            of
                Just topLeftCornerCell ->
                    List.foldl
                        (\subgridCell nexTilemap ->
                            setSuperpositionOptions subgridCell
                                (resetSuperposition subgridCell decorativeTiles nexTilemap)
                                nexTilemap
                        )
                        tilemap
                        (Tile.largeTileCells tilemapContents.config topLeftCornerCell largeTile)

                Nothing ->
                    tilemap

        TileConfig.Single _ ->
            tilemap


resetLargeTileAnchor : Cell -> ( TileId, Int ) -> Tilemap -> Tilemap
resetLargeTileAnchor subtileCell ( largeTileId, subtileIndex ) ((Tilemap tilemapContents) as tilemap) =
    case tileById largeTileId of
        TileConfig.Large largeTile ->
            largeTile
                |> Tile.largeTileTopLeftCell tilemapContents.config subtileCell subtileIndex
                |> Maybe.andThen (\cell -> largeTileAnchor cell tilemap largeTile)
                |> Maybe.andThen (Common.applyTuple2 (resetLotEntryByLargeTileAnchor tilemap))
                |> Maybe.withDefault tilemap

        TileConfig.Single _ ->
            tilemap


resetLotEntryByLargeTileAnchor : Tilemap -> Cell -> TileConfig.SingleTile -> Maybe Tilemap
resetLotEntryByLargeTileAnchor tilemap anchorCell anchorTile =
    directionBySocket anchorTile.sockets lotDrivewaySocket
        |> Maybe.andThen
            (\directionToLotEntry ->
                tileNeighborIn directionToLotEntry anchorCell fixedTileByCell tilemap
            )
        |> Maybe.andThen (Common.applyTuple2 (resetLotEntry tilemap))


resetLotEntry : Tilemap -> Cell -> Tile -> Maybe Tilemap
resetLotEntry tilemap lotEntryCell lotEntryTile =
    tileToConfig lotEntryTile
        |> Maybe.andThen TileConfig.baseTileId
        |> Maybe.map
            (\baseTileId ->
                let
                    tileConfig =
                        tileById baseTileId

                    ( nextTilemap, _ ) =
                        addTileFromWfc Nothing tileConfig lotEntryCell tilemap
                in
                nextTilemap
            )


largeTileAnchor : Cell -> Tilemap -> TileConfig.LargeTile -> Maybe ( Cell, TileConfig.SingleTile )
largeTileAnchor topLeftCornerCell (Tilemap tilemapContents) largeTile =
    let
        subgridDimensions =
            { horizontalCellsAmount = largeTile.width
            , verticalCellsAmount = largeTile.height
            }

        anchorTile =
            Array.get largeTile.anchorIndex largeTile.tiles

        anchorCell =
            Cell.fromArray1DIndex subgridDimensions largeTile.anchorIndex

        anchorCellInGlobalCoordinates =
            anchorCell |> Maybe.andThen (Cell.placeIn tilemapContents.config topLeftCornerCell)
    in
    Maybe.map2 Tuple.pair anchorCellInGlobalCoordinates anchorTile



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
removeAnchor lotId ((Tilemap tilemapContents) as tilemap) =
    let
        anchor =
            Dict.find
                (\_ ( anchorLotId, _ ) -> anchorLotId == lotId)
                tilemapContents.anchors
    in
    anchor
        |> Maybe.map Tuple.first
        |> Maybe.andThen (Cell.fromCoordinates tilemapContents.config)
        |> Maybe.map
            (\cell ->
                let
                    cellCoordinates =
                        Cell.coordinates cell
                in
                Tilemap { tilemapContents | anchors = Dict.remove cellCoordinates tilemapContents.anchors }
            )
        |> Maybe.withDefault tilemap


anchorByCell : Tilemap -> Cell -> Maybe ( Id, OrthogonalDirection )
anchorByCell tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap
    in
    Dict.get (Cell.coordinates cell) tilemapContents.anchors
