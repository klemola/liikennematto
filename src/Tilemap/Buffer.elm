module Tilemap.Buffer exposing
    ( reconcileSavedNatureTiles
    , registerPlacement
    , removeBuffer
    , revertSavedNature
    , roadBuildingInProgress
    , updateBufferCells
    )

import Data.TileSet exposing (connectionsByTile, tileById)
import Dict
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import Quantity exposing (Unitless)
import Set
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addTileFromTrail
        , clearSavedNature
        , clearTile
        , extractRoadTile
        , getBuildHistory
        , getSavedNatureAnchors
        , getSavedNatureTiles
        , getTilemapConfig
        , insertSavedNatureAnchor
        , insertSavedNatureTile
        , mapCell
        , removeSavedNatureAnchor
        , removeSavedNatureTile
        , roadTileFromCell
        , setBuildHistory
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (TileId)
import Vector2d exposing (Vector2d)


maxBufferDepth : Int
maxBufferDepth =
    3


{-| The stale check runs before trail capture so that capture never includes
history from a previous building phase. A placement that doesn't touch the
build history means the player moved elsewhere.
-}
registerPlacement : Cell -> Tilemap -> Tilemap
registerPlacement builtCell tilemap =
    let
        continuesBuildPhase =
            List.any (Cell.isAdjacent builtCell) (getBuildHistory tilemap)

        preparedTilemap =
            if continuesBuildPhase then
                tilemap

            else
                tilemap
                    |> clearSavedNature
                    |> setBuildHistory []
    in
    preparedTilemap
        |> captureTrail (trailCells builtCell preparedTilemap)
        |> updateBufferCells builtCell


updateBufferCells : Cell -> Tilemap -> Tilemap
updateBufferCells builtCell tilemap =
    let
        updatedHistory =
            nextCellHistory builtCell (getBuildHistory tilemap)

        withUpdatedHistory =
            setBuildHistory updatedHistory tilemap
    in
    List.foldl
        (\cell nextTilemap ->
            mapCell cell
                (\current ->
                    case current.kind of
                        Unintialized ->
                            Tile.init Tile.Buffer

                        _ ->
                            current
                )
                nextTilemap
        )
        withUpdatedHistory
        (bufferCells builtCell withUpdatedHistory)


removeBuffer : Cell -> Tilemap -> Tilemap
removeBuffer clearedCell tilemap =
    let
        withUpdatedHistory =
            setBuildHistory [] tilemap

        cellsToClear =
            case roadDirectionByNeighbors clearedCell tilemap of
                DirectionContinuous neighbor1 neighbor2 straightRoadDirection_ ->
                    List.concat
                        [ orphanCells clearedCell straightRoadDirection_ False
                        , orphanCells neighbor1.cell straightRoadDirection_ True
                        , orphanCells neighbor2.cell straightRoadDirection_ True
                        ]

                DirectionDeadend neighbor deadendDirection ->
                    orphanCells neighbor.cell deadendDirection True

                NoDirection ->
                    []

        orphanCells cell dir isDeadend =
            List.filterMap
                (extractOrphanBufferCell cell withUpdatedHistory isDeadend)
                (directionalBuffer cell dir ( maxBufferDepth, 0 ) withUpdatedHistory)
    in
    List.foldl
        clearTile
        withUpdatedHistory
        cellsToClear



--
-- Buffer generation
--


bufferCells : Cell -> Tilemap -> List Cell
bufferCells builtCell tilemap =
    case roadNeighbors builtCell tilemap of
        [ n1, n2 ] ->
            case straightRoadDirection n1 n2 of
                -- Straight road
                Just straightRoadDirection_ ->
                    List.concat
                        [ directionalBuffer builtCell straightRoadDirection_ ( maxBufferDepth, 0 ) tilemap
                        , neighborCellBuffer n1.cell tilemap
                        , neighborCellBuffer n2.cell tilemap
                        ]

                Nothing ->
                    -- Curve
                    List.concat
                        [ neighborCellBuffer n1.cell tilemap
                        , neighborCellBuffer n2.cell tilemap
                        ]

        [ n1 ] ->
            -- Deadend
            neighborCellBuffer n1.cell tilemap

        _ ->
            -- Intersection or standalone road
            []


neighborCellBuffer : Cell -> Tilemap -> List Cell
neighborCellBuffer neighborCell tilemap =
    case roadDirectionByNeighbors neighborCell tilemap of
        DirectionContinuous _ _ straightRoadDirection_ ->
            directionalBuffer neighborCell straightRoadDirection_ ( 3, 0 ) tilemap

        _ ->
            []


directionalBuffer : Cell -> OrthogonalDirection -> ( Int, Int ) -> Tilemap -> List Cell
directionalBuffer origin direction ( sideDepth, backwardDepth ) tilemap =
    let
        ( originX, originY ) =
            Cell.coordinates origin

        ( forwardVector, rightVector ) =
            case direction of
                Right ->
                    ( vector2dFromInt ( 1, 0 ), vector2dFromInt ( 0, 1 ) )

                Left ->
                    ( vector2dFromInt ( -1, 0 ), vector2dFromInt ( 0, -1 ) )

                Down ->
                    ( vector2dFromInt ( 0, 1 ), vector2dFromInt ( 1, 0 ) )

                Up ->
                    ( vector2dFromInt ( 0, -1 ), vector2dFromInt ( -1, 0 ) )

        backwardVector =
            Vector2d.reverse forwardVector

        subgridCorners : List CellCoordinates
        subgridCorners =
            [ ( originX, originY )
            , Cell.translateByVector (Vector2d.scaleBy (toFloat backwardDepth) backwardVector) origin
            , Cell.translateByVector (Vector2d.scaleBy (toFloat sideDepth) rightVector) origin
            , Cell.translateByVector (Vector2d.scaleBy (toFloat -sideDepth) rightVector) origin
            ]

        areaBounds =
            List.foldl
                (\( cornerX, cornerY ) { minX, minY, maxX, maxY } ->
                    { minX = min cornerX minX
                    , maxX = max cornerX maxX
                    , minY = min cornerY minY
                    , maxY = max cornerY maxY
                    }
                )
                { minX = originX, maxX = originX, minY = originY, maxY = originY }
                subgridCorners
    in
    Cell.fromArea (getTilemapConfig tilemap) areaBounds
        |> List.filter (\cell -> cell /= origin)



--
-- Buffer removal
--


extractOrphanBufferCell : Cell -> Tilemap -> Bool -> Cell -> Maybe Cell
extractOrphanBufferCell origin tilemap isDeadend maybeBufferCell =
    let
        hasOverlappingBuffer depth directionToOrigin neighborMeta =
            if depth > maxBufferDepth then
                False

            else if directionToOrigin == neighborMeta.direction && isDeadend then
                False

            else
                case extractRoadTile neighborMeta.tile of
                    Just road ->
                        case roadConnectionDirections road of
                            [ dir1, dir2 ] ->
                                -- Dirs on the same axis (straight road)
                                dir1 == OrthogonalDirection.opposite dir2

                            _ ->
                                False

                    Nothing ->
                        case toNeighbor neighborMeta.direction neighborMeta.cell tilemap of
                            Nothing ->
                                False

                            Just nextNeighbor ->
                                hasOverlappingBuffer (depth + 1) directionToOrigin nextNeighbor
    in
    Maybe.andThen
        (\directionToOrigin ->
            case tileByCell tilemap maybeBufferCell |> Maybe.map .kind of
                Just Buffer ->
                    if
                        List.any
                            (hasOverlappingBuffer 1 directionToOrigin)
                            (neighbors maybeBufferCell tilemap)
                    then
                        Nothing

                    else
                        Just maybeBufferCell

                _ ->
                    Nothing
        )
        (Cell.orthogonalDirection maybeBufferCell origin)



--
-- Helpers
--


vector2dFromInt : ( Int, Int ) -> Vector2d Unitless coordinates
vector2dFromInt ( x, y ) =
    Vector2d.unitless (toFloat x) (toFloat y)


nextCellHistory : Cell -> List Cell -> List Cell
nextCellHistory builtCell history =
    case history of
        previous :: _ ->
            if Cell.isAdjacent builtCell previous then
                (builtCell :: history) |> List.take 5

            else
                [ builtCell ]

        _ ->
            [ builtCell ]


type RoadDirection
    = DirectionContinuous NeighborMeta NeighborMeta OrthogonalDirection
    | DirectionDeadend NeighborMeta OrthogonalDirection
    | NoDirection


roadDirectionByNeighbors : Cell -> Tilemap -> RoadDirection
roadDirectionByNeighbors origin tilemap =
    let
        roadDirection =
            case roadNeighbors origin tilemap of
                [ n1, n2 ] ->
                    straightRoadDirection n1 n2 |> Maybe.map (DirectionContinuous n1 n2)

                [ n1 ] ->
                    Cell.orthogonalDirection origin n1.cell |> Maybe.map (DirectionDeadend n1)

                _ ->
                    Nothing
    in
    roadDirection |> Maybe.withDefault NoDirection


type alias NeighborMeta =
    { cell : Cell
    , tile : Tile
    , direction : OrthogonalDirection
    }


roadBuildingInProgress : Tilemap -> Bool
roadBuildingInProgress tilemap =
    let
        history =
            getBuildHistory tilemap

        historyCoords =
            history
                |> List.map Cell.coordinates
                |> Set.fromList

        connectsOutside cell =
            roadNeighbors cell tilemap
                |> List.any
                    (\n -> not (Set.member (Cell.coordinates n.cell) historyCoords))
    in
    List.any connectsOutside history


roadNeighbors : Cell -> Tilemap -> List NeighborMeta
roadNeighbors cell tilemap =
    Cell.orthogonalNeighbors (getTilemapConfig tilemap) cell
        |> List.filterMap
            (\( dir, neighbor ) ->
                Maybe.map
                    (\roadTile ->
                        { cell = neighbor
                        , tile = roadTile
                        , direction = dir
                        }
                    )
                    (roadTileFromCell neighbor tilemap)
            )


neighbors : Cell -> Tilemap -> List NeighborMeta
neighbors =
    filteredNeighbors
        (\dir neighbor tile ->
            case tile.kind of
                Unintialized ->
                    Nothing

                _ ->
                    Just
                        { cell = neighbor
                        , tile = tile
                        , direction = dir
                        }
        )


filteredNeighbors : (OrthogonalDirection -> Cell -> Tile -> Maybe NeighborMeta) -> Cell -> Tilemap -> List NeighborMeta
filteredNeighbors filterFn cell tilemap =
    cell
        |> Cell.orthogonalNeighbors (getTilemapConfig tilemap)
        |> List.filterMap
            (\( dir, neighbor ) ->
                tileByCell tilemap neighbor
                    |> Maybe.andThen (filterFn dir neighbor)
            )


toNeighbor : OrthogonalDirection -> Cell -> Tilemap -> Maybe NeighborMeta
toNeighbor dir origin tilemap =
    origin
        |> Cell.nextOrthogonalCell (getTilemapConfig tilemap) dir
        |> Maybe.andThen
            (\neighborCell ->
                Maybe.map
                    (\tile ->
                        { cell = neighborCell
                        , tile = tile
                        , direction = dir
                        }
                    )
                    (tileByCell tilemap neighborCell)
            )


straightRoadDirection : NeighborMeta -> NeighborMeta -> Maybe OrthogonalDirection
straightRoadDirection n1 n2 =
    Cell.orthogonalDirection n1.cell n2.cell


roadConnectionDirections : Tile -> List OrthogonalDirection
roadConnectionDirections tile =
    case Tile.id tile of
        Just tileId ->
            tileById tileId
                |> connectionsByTile
                |> .roadConnections

        Nothing ->
            []



--
-- Buffer trail (revert side strips of recent straight road cells)
--


trailCells : Cell -> Tilemap -> List Cell
trailCells builtCell tilemap =
    case roadNeighbors builtCell tilemap of
        [] ->
            []

        [ _ ] ->
            -- Continuation: the placed cell extends a road
            getBuildHistory tilemap
                |> List.take maxBufferDepth
                |> List.concatMap (sideStripIfStraight tilemap)

        manyNeighbors ->
            -- Join: the placed cell connects to at least 2 road neighbors. Walk along each
            -- neighbor direction, collecting side strips of straight cells
            manyNeighbors
                |> List.concatMap (\nm -> walkAxis tilemap nm.direction nm.cell trailJoinDepth)
                |> List.concatMap (sideStripIfStraight tilemap)


trailJoinDepth : Int
trailJoinDepth =
    maxBufferDepth


sideStripIfStraight : Tilemap -> Cell -> List Cell
sideStripIfStraight tilemap cell =
    case roadDirectionByNeighbors cell tilemap of
        DirectionContinuous _ _ axis ->
            directionalBuffer cell axis ( maxBufferDepth, 0 ) tilemap

        _ ->
            []


walkAxis : Tilemap -> OrthogonalDirection -> Cell -> Int -> List Cell
walkAxis tilemap dir cell budget =
    if budget <= 0 then
        []

    else
        case roadTileFromCell cell tilemap of
            Nothing ->
                []

            Just _ ->
                case Cell.nextOrthogonalCell (getTilemapConfig tilemap) dir cell of
                    Just nextCell ->
                        cell :: walkAxis tilemap dir nextCell (budget - 1)

                    Nothing ->
                        [ cell ]


captureTrail : List Cell -> Tilemap -> Tilemap
captureTrail cells tilemap =
    List.foldl captureTrailCell tilemap cells


captureTrailCell : Cell -> Tilemap -> Tilemap
captureTrailCell cell tilemap =
    case tileByCell tilemap cell |> Maybe.map .kind of
        Just (Fixed props) ->
            case props.parentTile of
                Nothing ->
                    if isSingleNatureTile props.id then
                        insertSavedNatureTile (Cell.coordinates cell) props.id tilemap

                    else
                        tilemap

                Just ( largeTileId, subgridIndex ) ->
                    captureLargeNatureTileAnchor cell largeTileId subgridIndex tilemap

        _ ->
            tilemap


{-| One member cell in the trail captures the whole large tile, keyed by its
top-left cell. Nature tiles larger than 1x2 are not captured.
-}
captureLargeNatureTileAnchor : Cell -> TileId -> Int -> Tilemap -> Tilemap
captureLargeNatureTileAnchor cell largeTileId subgridIndex tilemap =
    case tileById largeTileId of
        TileConfig.Large largeTile ->
            if largeTile.biome == TileConfig.Nature && largeTile.width * largeTile.height <= 2 then
                case Tile.largeTileTopLeftCell (getTilemapConfig tilemap) cell subgridIndex largeTile of
                    Just topLeftCell ->
                        insertSavedNatureAnchor (Cell.coordinates topLeftCell) largeTileId tilemap

                    Nothing ->
                        tilemap

            else
                tilemap

        TileConfig.Single _ ->
            tilemap


revertSavedNature : Tilemap -> Tilemap
revertSavedNature tilemap =
    tilemap
        |> revertSavedNatureSingleTiles
        |> revertSavedNatureAnchors


{-| Captured cells may have been built over since the capture (roads can be
placed on nature tiles). Reverting only applies while the cell holds no player
content; stale entries are dropped so the reconcile pass cannot restore nature
over the player's tiles either.
-}
revertSavedNatureSingleTiles : Tilemap -> Tilemap
revertSavedNatureSingleTiles tilemap =
    getSavedNatureTiles tilemap
        |> Dict.foldl
            (\coords _ acc ->
                case Cell.fromCoordinates (getTilemapConfig acc) coords of
                    Just cell ->
                        if revertableSingle cell acc then
                            revertToBuffer cell acc

                        else
                            removeSavedNatureTile coords acc

                    Nothing ->
                        acc
            )
            tilemap


revertableSingle : Cell -> Tilemap -> Bool
revertableSingle cell tilemap =
    case tileByCell tilemap cell |> Maybe.map .kind of
        Just (Fixed props) ->
            props.parentTile == Nothing && isSingleNatureTile props.id

        Just _ ->
            True

        Nothing ->
            False


revertSavedNatureAnchors : Tilemap -> Tilemap
revertSavedNatureAnchors tilemap =
    getSavedNatureAnchors tilemap
        |> Dict.foldl revertAnchorEntry tilemap


{-| Building over any member removes the whole large tile, so one non-member
cell means the capture is stale.
-}
revertAnchorEntry : CellCoordinates -> TileId -> Tilemap -> Tilemap
revertAnchorEntry coords largeTileId tilemap =
    case ( Cell.fromCoordinates (getTilemapConfig tilemap) coords, tileById largeTileId ) of
        ( Just topLeftCell, TileConfig.Large largeTile ) ->
            let
                footprint =
                    Tile.largeTileCells (getTilemapConfig tilemap) topLeftCell largeTile
            in
            if List.all (stillMemberOf largeTileId tilemap) footprint then
                List.foldl revertToBuffer tilemap footprint

            else
                removeSavedNatureAnchor coords tilemap

        _ ->
            removeSavedNatureAnchor coords tilemap


stillMemberOf : TileId -> Tilemap -> Cell -> Bool
stillMemberOf largeTileId tilemap cell =
    case tileByCell tilemap cell |> Maybe.map .kind of
        Just (Fixed props) ->
            case props.parentTile of
                Just ( parentId, _ ) ->
                    parentId == largeTileId

                Nothing ->
                    False

        _ ->
            False


revertToBuffer : Cell -> Tilemap -> Tilemap
revertToBuffer cell tilemap =
    mapCell cell (\_ -> Tile.init Tile.Buffer) tilemap


isSingleNatureTile : TileId -> Bool
isSingleNatureTile tileId =
    case tileById tileId of
        TileConfig.Single singleTile ->
            singleTile.biome == TileConfig.Nature

        TileConfig.Large _ ->
            False


{-| The dicts never share cells (a cell was either a single or a large tile
member at capture), so the pass order doesn't matter.
-}
reconcileSavedNatureTiles : Tilemap -> Tilemap
reconcileSavedNatureTiles tilemap =
    tilemap
        |> reconcileAnchors
        |> reconcileSingleTiles
        |> clearSavedNature


reconcileSingleTiles : Tilemap -> Tilemap
reconcileSingleTiles tilemap =
    getSavedNatureTiles tilemap
        |> Dict.foldl (reconcileEntry (getTilemapConfig tilemap)) tilemap


reconcileAnchors : Tilemap -> Tilemap
reconcileAnchors tilemap =
    getSavedNatureAnchors tilemap
        |> Dict.foldl (reconcileAnchorEntry (getTilemapConfig tilemap)) tilemap


reconcileAnchorEntry : TilemapConfig -> CellCoordinates -> TileId -> Tilemap -> Tilemap
reconcileAnchorEntry constraints coords largeTileId tilemap =
    case ( Cell.fromCoordinates constraints coords, tileById largeTileId ) of
        ( Just topLeftCell, TileConfig.Large largeTile ) ->
            let
                footprint =
                    Tile.largeTileCells constraints topLeftCell largeTile
            in
            if List.any (footprintCellClaimedByLargeTile tilemap) footprint then
                tilemap

            else
                reinstateLargeTile constraints topLeftCell largeTile tilemap

        _ ->
            tilemap


footprintCellClaimedByLargeTile : Tilemap -> Cell -> Bool
footprintCellClaimedByLargeTile tilemap cell =
    case tileByCell tilemap cell |> Maybe.map .kind of
        Just (Fixed { parentTile }) ->
            parentTile /= Nothing

        _ ->
            False


{-| Nature tile outer sockets are uniform, so overwriting the footprint keeps
neighbors valid.
-}
reinstateLargeTile : TilemapConfig -> Cell -> TileConfig.LargeTile -> Tilemap -> Tilemap
reinstateLargeTile constraints topLeftCell largeTile tilemap =
    List.foldl
        (\( globalCell, subgridIndex, singleTile ) acc ->
            mapCell globalCell (\_ -> memberTile largeTile.id subgridIndex singleTile) acc
        )
        tilemap
        (Tile.largeTileContent constraints topLeftCell largeTile)


memberTile : TileId -> Int -> TileConfig.SingleTile -> Tile
memberTile largeTileId subgridIndex singleTile =
    Tile.fromTileConfig
        (TileConfig.Single singleTile)
        (Just ( largeTileId, subgridIndex ))
        Tile.RestoreFromTrail
        |> Tuple.first


reconcileEntry : TilemapConfig -> CellCoordinates -> TileId -> Tilemap -> Tilemap
reconcileEntry constraints coords savedId tilemap =
    case Cell.fromCoordinates constraints coords of
        Nothing ->
            tilemap

        Just cell ->
            case tileByCell tilemap cell of
                Just tile ->
                    case tile.kind of
                        Fixed props ->
                            if props.parentTile /= Nothing then
                                -- Lot subgrid; leave as-is.
                                tilemap

                            else
                                case tileById props.id of
                                    TileConfig.Large _ ->
                                        tilemap

                                    TileConfig.Single singleTile ->
                                        if singleTile.biome == TileConfig.Lot then
                                            tilemap

                                        else if props.id == savedId then
                                            tilemap

                                        else
                                            restoreCell cell savedId tilemap

                        Buffer ->
                            restoreCell cell savedId tilemap

                        Superposition _ ->
                            restoreCell cell savedId tilemap

                        Unintialized ->
                            tilemap

                Nothing ->
                    tilemap


restoreCell : Cell -> TileId -> Tilemap -> Tilemap
restoreCell cell tileId tilemap =
    let
        ( nextTilemap, _ ) =
            addTileFromTrail (tileById tileId) cell tilemap
    in
    nextTilemap
