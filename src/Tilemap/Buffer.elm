module Tilemap.Buffer exposing
    ( reconcileSavedNatureTiles
    , removeBuffer
    , revertTrailToBuffer
    , trailCells
    , updateBufferCells
    )

import Data.TileSet exposing (connectionsByTile, tileById)
import Dict
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import Quantity exposing (Unitless)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addTileFromTrail
        , clearTile
        , extractRoadTile
        , getBuildHistory
        , getSavedNatureTiles
        , getTilemapConfig
        , insertSavedNatureTile
        , mapCell
        , roadTileFromCell
        , setBuildHistory
        , setSavedNatureTiles
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (TileId)
import Vector2d exposing (Vector2d)


maxBufferDepth : Int
maxBufferDepth =
    3


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
            -- Continuation: the placed cell extends a road. Walk recent build history.
            getBuildHistory tilemap
                |> List.take maxBufferDepth
                |> List.concatMap (sideStripIfStraight tilemap)
                |> dedupeCells

        manyNeighbors ->
            -- Join: the placed cell connects to ≥2 road neighbors. Walk along each
            -- neighbor direction up to 2 cells, collecting side strips of straight cells.
            manyNeighbors
                |> List.concatMap (\nm -> walkAxis tilemap nm.direction nm.cell trailJoinDepth)
                |> List.concatMap (sideStripIfStraight tilemap)
                |> dedupeCells


trailJoinDepth : Int
trailJoinDepth =
    2


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
                cell
                    :: (case Cell.nextOrthogonalCell (getTilemapConfig tilemap) dir cell of
                            Just nextCell ->
                                walkAxis tilemap dir nextCell (budget - 1)

                            Nothing ->
                                []
                       )


dedupeCells : List Cell -> List Cell
dedupeCells cells =
    cells
        |> List.foldl
            (\cell ( seen, acc ) ->
                let
                    coords =
                        Cell.coordinates cell
                in
                if List.member coords seen then
                    ( seen, acc )

                else
                    ( coords :: seen, cell :: acc )
            )
            ( [], [] )
        |> Tuple.second


revertTrailToBuffer : List Cell -> Tilemap -> Tilemap
revertTrailToBuffer cells tilemap =
    List.foldl revertTrailCell tilemap cells


revertTrailCell : Cell -> Tilemap -> Tilemap
revertTrailCell cell tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            case tile.kind of
                Fixed props ->
                    if props.parentTile == Nothing && isSingleNatureTile props.id then
                        tilemap
                            |> insertSavedNatureTile (Cell.coordinates cell) props.id
                            |> mapCell cell (\_ -> Tile.init Tile.Buffer)

                    else
                        tilemap

                _ ->
                    tilemap

        Nothing ->
            tilemap


isSingleNatureTile : TileId -> Bool
isSingleNatureTile tileId =
    case tileById tileId of
        TileConfig.Single singleTile ->
            singleTile.biome == TileConfig.Nature

        TileConfig.Large _ ->
            False


reconcileSavedNatureTiles : Tilemap -> Tilemap
reconcileSavedNatureTiles tilemap =
    let
        constraints =
            getTilemapConfig tilemap

        ( restoredTilemap, remainingDict ) =
            getSavedNatureTiles tilemap
                |> Dict.foldl
                    (reconcileEntry constraints)
                    ( tilemap, Dict.empty )
    in
    setSavedNatureTiles remainingDict restoredTilemap


reconcileEntry :
    TilemapConfig
    -> CellCoordinates
    -> TileId
    -> ( Tilemap, Dict.Dict CellCoordinates TileId )
    -> ( Tilemap, Dict.Dict CellCoordinates TileId )
reconcileEntry constraints coords savedId ( tilemap, dict ) =
    case Cell.fromCoordinates constraints coords of
        Nothing ->
            ( tilemap, dict )

        Just cell ->
            case tileByCell tilemap cell of
                Just tile ->
                    case tile.kind of
                        Fixed props ->
                            if props.parentTile /= Nothing then
                                -- Lot subgrid; leave as-is.
                                ( tilemap, dict )

                            else
                                case tileById props.id of
                                    TileConfig.Large _ ->
                                        ( tilemap, dict )

                                    TileConfig.Single singleTile ->
                                        if singleTile.biome == TileConfig.Lot then
                                            ( tilemap, dict )

                                        else if props.id == savedId then
                                            ( tilemap, dict )

                                        else
                                            ( restoreCell cell savedId tilemap, dict )

                        Buffer ->
                            ( restoreCell cell savedId tilemap, dict )

                        Superposition _ ->
                            ( restoreCell cell savedId tilemap, dict )

                        Unintialized ->
                            ( tilemap, dict )

                Nothing ->
                    ( tilemap, dict )


restoreCell : Cell -> TileId -> Tilemap -> Tilemap
restoreCell cell tileId tilemap =
    let
        ( nextTilemap, _ ) =
            addTileFromTrail (tileById tileId) cell tilemap
    in
    nextTilemap
