module Tilemap.Buffer exposing (DirectionHistory, removeBuffer, updateBufferCells)

import Data.TileSet exposing (decorativeTiles, lotTiles, roadConnectionDirectionsByTile, tileById)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import Quantity exposing (Unitless)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , clearTile
        , extractRoadTile
        , getBuildHistory
        , getTilemapConfig
        , resetTileBySurroundings
        , roadTile
        , setBuildHistory
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Vector2d exposing (Vector2d)


type alias DirectionHistory =
    List OrthogonalDirection


maxBufferDepth : Int
maxBufferDepth =
    3


updateBufferCells : Cell -> Tilemap -> Tilemap
updateBufferCells newCell tilemap =
    let
        updatedHistory =
            nextCellHistory newCell (getBuildHistory tilemap)

        withUpdatedHistory =
            setBuildHistory updatedHistory tilemap
    in
    List.foldl
        (\cell nextTilemap ->
            resetTileBySurroundings cell (decorativeTiles ++ lotTiles) nextTilemap
        )
        withUpdatedHistory
        (bufferCellsFromHistory withUpdatedHistory)


removeBuffer : Cell -> Tilemap -> Tilemap
removeBuffer removedCell tilemap =
    let
        withUpdatedHistory =
            setBuildHistory [] tilemap

        cellsToClear =
            case roadDirectionByNeighbors removedCell tilemap of
                DirectionContinuous neighbor1 neighbor2 straightRoadDirection_ ->
                    List.concat
                        [ orphanCells removedCell straightRoadDirection_ False
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


bufferCellsFromHistory : Tilemap -> List Cell
bufferCellsFromHistory tilemap =
    case getBuildHistory tilemap of
        current :: _ ->
            case roadNeighbors current tilemap of
                [ n1, n2 ] ->
                    case straightRoadDirection n1 n2 of
                        -- Straight road
                        Just straightRoadDirection_ ->
                            List.concat
                                [ directionalBuffer current straightRoadDirection_ ( maxBufferDepth, 0 ) tilemap
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

        [] ->
            -- Should not happen
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
                case roadTile neighborMeta.tile of
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
                Just (Superposition _) ->
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
nextCellHistory newCell history =
    case history of
        previous :: _ ->
            if Cell.isAdjacent newCell previous then
                (newCell :: history) |> List.take 3

            else
                [ newCell ]

        _ ->
            [ newCell ]


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
                    (extractRoadTile neighbor tilemap)
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
            tileById tileId |> roadConnectionDirectionsByTile

        Nothing ->
            []
