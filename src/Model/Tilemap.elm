module Model.Tilemap exposing
    ( TileListFilter(..)
    , Tilemap
    , TilemapConfig
    , TilemapUpdateResult
    , addAnchor
    , addTile
    , all
    , anchorAt
    , boundingBox
    , canBuildRoadAt
    , config
    , dimensions
    , empty
    , exists
    , fold
    , fromCells
    , hasAnchor
    , inBounds
    , intersects
    , removeAnchor
    , removeTile
    , setTile
    , size
    , tileAt
    , tileAtAny
    , toList
    , update
    )

import Array exposing (Array)
import Array.Extra as Array
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
        , TileKind(..)
        , TileOperation
        , chooseTileKind
        )
import Model.TileConfig exposing (TileConfig, TileId, tileConfigId)
import Point2d
import Quantity
import Random


type Tilemap
    = Tilemap
        { cells : Array Tile
        , anchors : Dict CellCoordinates ( Id, OrthogonalDirection )
        , width : Length
        , height : Length
        , boundingBox : LMBoundingBox2d
        , config : TilemapConfig
        }


type alias TilemapConfig =
    { horizontalCellsAmount : Int
    , verticalCellsAmount : Int
    , initialSeed : Random.Seed
    , defaultTile : TileConfig
    , tiles : List TileConfig
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

        tileFill =
            Tile.init (superposition tilemapConfig.tiles)
    in
    Tilemap
        { cells = Array.initialize arrSize (always tileFill)
        , anchors = Dict.empty
        , width = width
        , height = height
        , boundingBox = Common.boundingBoxWithDimensions width height Point2d.origin
        , config = tilemapConfig
        }


superposition : List TileConfig -> TileKind
superposition tileConfigs =
    tileConfigs
        |> List.map tileConfigId
        |> Superposition


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
        |> Maybe.andThen extractFixedTile


tileAtAny : Tilemap -> Cell -> Maybe Tile
tileAtAny tilemap cell =
    let
        (Tilemap tilemapContents) =
            tilemap

        idx =
            indexFromCell tilemap cell
    in
    Array.get idx tilemapContents.cells


extractFixedTile : Tile -> Maybe Tile
extractFixedTile tile =
    case tile.kind of
        Fixed _ ->
            Just tile

        Superposition _ ->
            Nothing


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
    -- FIXME : upoptimized, skips dynamic tiles
    toList (\cell _ -> Cell.boundingBox cell) StaticTiles tilemap
        |> List.any (Common.boundingBoxOverlaps testBB)


exists : Cell -> Tilemap -> Bool
exists cell tilemap =
    tileAt tilemap cell
        |> Maybe.andThen extractFixedTile
        |> Maybe.isJust


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
                (\tile { acc, index } ->
                    { acc =
                        cellFromIndex tilemap index
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


fold : (Cell -> Tile -> b -> b) -> b -> Tilemap -> b
fold foldFn b tilemap =
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
                        cellFromIndex tilemap index
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


all : (Tile -> Bool) -> Tilemap -> Bool
all predicate (Tilemap tilemapContents) =
    -- Room for improvement: early exit if any cell does not satisfy the predicate
    Array.all predicate tilemapContents.cells


config : Tilemap -> TilemapConfig
config (Tilemap tilemapContents) =
    tilemapContents.config


size : Tilemap -> Int
size (Tilemap tilemapContents) =
    Array.foldl
        (\tile count ->
            if not <| Tile.isPropagating tile then
                count + 1

            else
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
    Tilemap { tilemapContents | cells = tilemapContents.cells |> Array.set idx tile }


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
    xyZeroIndexed.x + (xyZeroIndexed.y * tilemapContents.config.verticalCellsAmount)



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


update : Duration -> Tilemap -> TilemapUpdateResult
update delta tilemap =
    let
        (Tilemap currentTilemap) =
            tilemap

        tilemapConfig =
            config tilemap

        cellsUpdate =
            Array.foldl
                (updateTile delta tilemapConfig)
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


updateTile : Duration -> TilemapConfig -> Tile -> TilemapUpdate -> TilemapUpdate
updateTile delta tilemapConfig tile tilemapUpdate =
    let
        idx =
            Array.length tilemapUpdate.nextTiles

        ( nextFSM, tileActions ) =
            FSM.updateWithoutContext delta tile.fsm

        isRemoved =
            FSM.toCurrentState nextFSM == Tile.Removed

        nextTile =
            if isRemoved then
                Tile.init (superposition tilemapConfig.tiles)

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
                    Tile.updateTileId anchorTileKind tile
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
