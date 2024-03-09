module Tilemap.Core exposing
    ( TileListFilter(..)
    , Tilemap
    , TilemapConfig
    , TilemapUpdateResult
    , addAnchor
    , addTile
    , anchorByCell
    , canBuildRoadAt
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
    , setSuperpositionOptions
    , setTile
    , tileByCell
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
import Common exposing (GlobalCoordinates)
import Data.TileSet as TileSet
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import Length exposing (Length)
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
        , chooseTileKind
        )
import Tilemap.TileConfig exposing (TileId)


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
    fromCellsHelper cells (createTilemap tilemapConfig)


fromCellsHelper : List Cell -> Tilemap -> Tilemap
fromCellsHelper remainingCells tilemap =
    case remainingCells of
        [] ->
            tilemap

        cell :: others ->
            let
                tilemapUpdateResult =
                    tilemap
                        |> applyTilemapOperation Tile.BuildInstantly cell
                        |> Tuple.first
                        -- run a FSM update cycle to make sure that tiles are not transitioning
                        |> updateTilemap (Duration.milliseconds 1000)
            in
            fromCellsHelper others tilemapUpdateResult.tilemap


initTile : TilemapConfig -> Int -> Tile
initTile tilemapConfig index =
    index
        |> Cell.fromArray1DIndexUnsafe tilemapConfig
        |> Cell.connectedBounds tilemapConfig
        |> Maybe.map boundarySuperposition
        |> Maybe.withDefault (Superposition TileSet.tileIds)
        |> Tile.init


boundarySuperposition : Cell.Boundary -> TileKind
boundarySuperposition boundary =
    Superposition
        (case boundary of
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
        )


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

        nextTilemap =
            List.foldl
                (\idx acc ->
                    case Cell.fromArray1DIndex currentTilemap.config idx of
                        Just cell ->
                            updateNeighborCells cell acc

                        Nothing ->
                            acc
                )
                (Tilemap { currentTilemap | cells = cellsUpdate.nextTiles })
                cellsUpdate.emptiedIndices

        transitionedCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.transitionedIndices

        dynamicCells =
            List.filterMap
                (Cell.fromArray1DIndex currentTilemap.config)
                cellsUpdate.dynamicIndices
    in
    { tilemap = nextTilemap
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


addTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
addTile =
    applyTilemapOperation Tile.Add


setTile : Cell -> Tile -> Tilemap -> Tilemap
setTile cell tile tilemap =
    updateCell cell tile tilemap


changeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
changeTile =
    applyTilemapOperation Tile.Change


removeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
removeTile origin tilemap =
    case
        fixedTileByCell tilemap origin
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
    case fixedTileByCell tilemap anchor of
        Just tile ->
            let
                anchorTileKind =
                    chooseTile tilemap anchor

                ( anchorTile, actions ) =
                    Tile.updateTileId anchorTileKind tile
            in
            ( updateCell anchor anchorTile tilemap, actions )

        Nothing ->
            ( tilemap, [] )


applyTilemapOperation : TileOperation -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation operation origin tilemap =
    let
        originTileKind =
            chooseTile tilemap origin

        ( originTile, initialActions ) =
            Tile.fromTileId originTileKind operation

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
                        Tile.updateTileId nextTileKind tile
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
            Tilemap { tilemapContents | anchors = nextAnchors }
    in
    setAnchorTile anchor tilemapWithAnchor |> Tuple.first


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

                tilemapWithAnchorRemoved =
                    Tilemap { tilemapContents | anchors = Dict.remove cellCoordinates tilemapContents.anchors }
            in
            case fixedTileByCell tilemap cell of
                Just _ ->
                    -- Temporarily ignore Tile actions. Revisit when the Lot FSM is designed.
                    changeTile cell tilemapWithAnchorRemoved |> Tuple.first

                Nothing ->
                    tilemapWithAnchorRemoved

        Nothing ->
            tilemap


anchorCell : Tilemap -> ( CellCoordinates, ( Id, OrthogonalDirection ) ) -> Maybe Cell
anchorCell tilemap ( cellCoordinates, _ ) =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    Cell.fromCoordinates tilemapConfig cellCoordinates


nextOrthogonalTile : OrthogonalDirection -> Cell -> Tilemap -> Maybe ( Cell, Tile )
nextOrthogonalTile dir cell tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap

        maybeCell =
            Cell.nextOrthogonalCell tilemapConfig dir cell

        maybeTile =
            maybeCell
                |> Maybe.andThen (fixedTileByCell tilemap)
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


chooseTile : Tilemap -> Cell -> TileId
chooseTile tilemap origin =
    let
        orthogonalNeighbors =
            { up = hasOrthogonalNeighborAt Up origin tilemap
            , left = hasOrthogonalNeighborAt Left origin tilemap
            , right = hasOrthogonalNeighborAt Right origin tilemap
            , down = hasOrthogonalNeighborAt Down origin tilemap
            }

        lotModifier =
            cellHasAnchor tilemap origin
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
    case anchorByCell tilemap anchor of
        Just ( _, dir ) ->
            dir == anchorDir

        Nothing ->
            False
