module Model.Tilemap exposing
    ( TileListFilter(..)
    , Tilemap
    , TilemapConfig
    , TilemapUpdateResult
    , addAnchor
    , addTile
    , anchorAt
    , boundingBox
    , canBuildRoadAt
    , config
    , dimensions
    , empty
    , exists
    , fromCells
    , hasAnchor
    , inBounds
    , intersects
    , removeAnchor
    , removeTile
    , size
    , tileAt
    , toList
    , update
    )

import Array exposing (Array)
import BoundingBox2d
import Collection exposing (Id)
import Common
import Dict exposing (Dict)
import Dict.Extra as Dict
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Model.Geometry
    exposing
        ( DiagonalDirection
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
        , horizontalCellsAmount : Int
        , verticalCellsAmount : Int
        , width : Length
        , height : Length
        , boundingBox : LMBoundingBox2d
        }


type alias TilemapConfig =
    { horizontalCellsAmount : Int
    , verticalCellsAmount : Int
    }


empty : TilemapConfig -> Tilemap
empty tilemapConfig =
    let
        width =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.horizontalCellsAmount)

        height =
            Cell.size |> Quantity.multiplyBy (toFloat tilemapConfig.verticalCellsAmount)

        arrSize =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount
    in
    Tilemap
        { cells = Array.initialize arrSize (always Nothing)
        , anchors = Dict.empty
        , horizontalCellsAmount = tilemapConfig.horizontalCellsAmount
        , verticalCellsAmount = tilemapConfig.verticalCellsAmount
        , width = width
        , height = height
        , boundingBox = Common.boundingBoxWithDimensions width height Point2d.origin
        }


fromCells : TilemapConfig -> List Cell -> Tilemap
fromCells tilemapConfig cells =
    fromCellsHelper cells (empty tilemapConfig)


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
                        |> update (Duration.milliseconds 1000)
            in
            fromCellsHelper others tilemapUpdateResult.tilemap


tileAt : Tilemap -> Cell -> Maybe Tile
tileAt tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            indexFromCell tilemap cell
    in
    Array.get idx tilemapContents.cells
        |> Maybe.andThen identity


anchorAt : Tilemap -> Cell -> Maybe ( Id, OrthogonalDirection )
anchorAt tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap
    in
    Dict.get (Cell.coordinates cell) tilemapContents.anchors


hasAnchor : Tilemap -> Cell -> Bool
hasAnchor (Tilemap tilemapContents) cell =
    Dict.member (Cell.coordinates cell) tilemapContents.anchors


inBounds : Tilemap -> LMBoundingBox2d -> Bool
inBounds (Tilemap tilemap) testBB =
    BoundingBox2d.isContainedIn tilemap.boundingBox testBB


intersects : LMBoundingBox2d -> Tilemap -> Bool
intersects testBB tilemap =
    toList (\cell _ -> Cell.boundingBox cell) NoFilter tilemap
        |> List.any (Common.boundingBoxOverlaps testBB)


exists : Cell -> Tilemap -> Bool
exists cell tilemap =
    tileAt tilemap cell |> Maybe.isJust


canBuildRoadAt : Cell -> Tilemap -> Bool
canBuildRoadAt cell tilemap =
    List.all (hasLowComplexity cell tilemap) diagonalDirections


hasLowComplexity : Cell -> Tilemap -> DiagonalDirection -> Bool
hasLowComplexity cell tilemap diagonalDirection =
    let
        tilemapConfig =
            config tilemap
    in
    Cell.quadrantNeighbors tilemapConfig diagonalDirection cell
        |> List.filterMap (tileAt tilemap)
        |> (\tiles -> List.length tiles < 3)


type TileListFilter
    = StaticTiles
    | NoFilter


toList : (Cell -> Tile -> a) -> TileListFilter -> Tilemap -> List a
toList mapperFn listFilter tilemap =
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
                (\maybeTile { acc, index } ->
                    { acc =
                        case maybeTile of
                            Just tile ->
                                cellFromIndex tilemap index
                                    |> Maybe.map
                                        (\cell ->
                                            if listFilter == StaticTiles && Tile.isDynamic tile then
                                                acc

                                            else
                                                mapperFn cell tile :: acc
                                        )
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


config : Tilemap -> TilemapConfig
config (Tilemap tilemapContents) =
    { verticalCellsAmount = tilemapContents.verticalCellsAmount
    , horizontalCellsAmount = tilemapContents.horizontalCellsAmount
    }


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


dimensions : Tilemap -> { width : Length, height : Length }
dimensions (Tilemap tilemapContents) =
    { width = tilemapContents.width
    , height = tilemapContents.height
    }


boundingBox : Tilemap -> LMBoundingBox2d
boundingBox (Tilemap tilemapContents) =
    tilemapContents.boundingBox


updateCell : Cell -> Tile -> Tilemap -> Tilemap
updateCell cell tile tilemap =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            indexFromCell tilemap cell
    in
    Tilemap { tilemapContents | cells = tilemapContents.cells |> Array.set idx (Just tile) }


cellFromIndex : Tilemap -> Int -> Maybe Cell
cellFromIndex tilemap idx =
    let
        tilemapConfig =
            config tilemap

        xyZeroIndexed =
            { x = remainderBy tilemapConfig.horizontalCellsAmount idx
            , y = idx // tilemapConfig.verticalCellsAmount
            }
    in
    -- Cells are 1-indexed - map the coordinates to match
    Cell.fromCoordinates tilemapConfig
        ( xyZeroIndexed.x + 1
        , xyZeroIndexed.y + 1
        )


indexFromCell : Tilemap -> Cell -> Int
indexFromCell (Tilemap tilemapContents) cell =
    let
        ( cellX, cellY ) =
            Cell.coordinates cell

        xyZeroIndexed =
            { x = cellX - 1
            , y = cellY - 1
            }
    in
    -- Arrays are 0-indexed - map the coordinates to match
    xyZeroIndexed.x + (xyZeroIndexed.y * tilemapContents.verticalCellsAmount)



--
-- Update
--


type alias TilemapUpdate =
    { nextTiles : Array (Maybe Tile)
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


update : Duration -> Tilemap -> TilemapUpdateResult
update delta tilemap =
    let
        (Tilemap currentTilemap) =
            tilemap

        cellsUpdate =
            Array.foldl
                (maybeUpdateTile delta)
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
                    case cellFromIndex tilemap idx of
                        Just cell ->
                            updateNeighborCells cell acc

                        Nothing ->
                            acc
                )
                (Tilemap { currentTilemap | cells = cellsUpdate.nextTiles })
                cellsUpdate.emptiedIndices

        transitionedCells =
            cellsUpdate.transitionedIndices
                |> List.map (cellFromIndex nextTilemap)
                |> Maybe.values

        dynamicCells =
            cellsUpdate.dynamicIndices
                |> List.map (cellFromIndex nextTilemap)
                |> Maybe.values
    in
    { tilemap = nextTilemap
    , actions = cellsUpdate.actions
    , transitionedCells = transitionedCells
    , dynamicCells = dynamicCells
    }


maybeUpdateTile : Duration -> Maybe Tile -> TilemapUpdate -> TilemapUpdate
maybeUpdateTile delta maybeTile tilemapUpdate =
    case maybeTile of
        Just tile ->
            updateTileFSM delta tile tilemapUpdate

        Nothing ->
            { nextTiles = tilemapUpdate.nextTiles |> Array.push Nothing
            , actions = tilemapUpdate.actions
            , emptiedIndices = tilemapUpdate.emptiedIndices
            , transitionedIndices = tilemapUpdate.transitionedIndices
            , dynamicIndices = tilemapUpdate.dynamicIndices
            }


updateTileFSM : Duration -> Tile -> TilemapUpdate -> TilemapUpdate
updateTileFSM delta tile tilemapUpdate =
    let
        idx =
            Array.length tilemapUpdate.nextTiles

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

        isDynamic =
            nextTile |> Maybe.map Tile.isDynamic |> Maybe.withDefault False

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
            if isDynamic then
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


changeTile : Cell -> Tilemap -> ( Tilemap, List Tile.Action )
changeTile =
    applyTilemapOperation Tile.Change


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


applyTilemapOperation : TileOperation -> Cell -> Tilemap -> ( Tilemap, List Tile.Action )
applyTilemapOperation tileChange origin tilemap =
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
            case tileAt tilemap cell of
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
            config tilemap
    in
    Cell.fromCoordinates tilemapConfig cellCoordinates


nextOrthogonalTile : OrthogonalDirection -> Cell -> Tilemap -> Maybe ( Cell, Tile )
nextOrthogonalTile dir cell tilemap =
    let
        tilemapConfig =
            config tilemap

        maybeCell =
            Cell.nextOrthogonalCell tilemapConfig dir cell

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
        Just ( _, dir ) ->
            dir == anchorDir

        Nothing ->
            False
