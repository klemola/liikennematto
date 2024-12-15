module Data.Utility exposing
    ( AnchorDef
    , addTileInstantly
    , cellsByTileKind
    , cellsByTileKindFromAscii
    , createCell
    , getStartAndEndNode
    , placeRoadAndUpdateBuffer
    , removeRoadAndUpdateBuffer
    , removeTileInstantly
    , tenByTenTilemap
    , testSeed
    , tilemapFromCoordinates
    , tilemapToAscii
    , worldFromTilemap
    )

import Data.TileSet exposing (tileIdByBitmask)
import Duration
import Lib.Collection exposing (Id)
import Lib.OrthogonalDirection exposing (OrthogonalDirection)
import Model.World as World exposing (World)
import Random
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Tilemap.Buffer exposing (removeBuffer, updateBufferCells)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addAnchor
        , cellBitmask
        , createTilemap
        , foldTiles
        , getTilemapConfig
        , removeTile
        , updateTilemap
        )
import Tilemap.Tile as Tile
import Tilemap.Update exposing (addTileById)


type alias AnchorDef =
    { lotId : Id
    , anchorDirection : OrthogonalDirection
    , cellCoordinates : Cell.CellCoordinates
    }


testSeed : Random.Seed
testSeed =
    Random.initialSeed 42


tenByTenTilemap : TilemapConfig
tenByTenTilemap =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


tilemapFromCoordinates : TilemapConfig -> List CellCoordinates -> List AnchorDef -> Tilemap
tilemapFromCoordinates tilemapConfig cellCoordinates anchorDefs =
    let
        cells =
            List.filterMap (Cell.fromCoordinates tenByTenTilemap) cellCoordinates

        withCells =
            tilemapFromCells tilemapConfig cells
    in
    addAnchors withCells anchorDefs


addAnchors : Tilemap -> List AnchorDef -> Tilemap
addAnchors tilemap anchorDefs =
    List.foldl
        (\{ cellCoordinates, lotId, anchorDirection } nextTilemap ->
            case Cell.fromCoordinates (getTilemapConfig tilemap) cellCoordinates of
                Just cell ->
                    addAnchor cell lotId anchorDirection nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        anchorDefs


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty (getTilemapConfig tilemap)
        |> World.setSeed testSeed


tilemapFromCells : TilemapConfig -> List Cell -> Tilemap
tilemapFromCells tilemapConfig cells =
    fromCellsHelper cells (createTilemap tilemapConfig (\_ -> Tile.init Tile.Unintialized))


fromCellsHelper : List Cell -> Tilemap -> Tilemap
fromCellsHelper remainingCells tilemap =
    case remainingCells of
        [] ->
            tilemap

        cell :: others ->
            fromCellsHelper others (addTileInstantly cell tilemap)


getStartAndEndNode : World -> Int -> Int -> Maybe ( RNNodeContext, RNNodeContext )
getStartAndEndNode world startId endId =
    let
        start =
            RoadNetwork.nodeById world.roadNetwork startId

        end =
            RoadNetwork.nodeById world.roadNetwork endId
    in
    Maybe.map2 Tuple.pair start end


addTileInstantly : Cell -> Tilemap -> Tilemap
addTileInstantly cell tilemap =
    let
        bitmask =
            cellBitmask cell tilemap

        ( tilemapWithTile, _ ) =
            case tileIdByBitmask bitmask of
                Just tileId ->
                    addTileById cell tileId (worldFromTilemap tilemap) tilemap

                Nothing ->
                    ( tilemap, [] )

        tilemapUpdateResult =
            -- run a FSM update cycle to make sure that tiles are not transitioning
            updateTilemap (Duration.milliseconds 260) tilemapWithTile
    in
    tilemapUpdateResult.tilemap


removeTileInstantly : Cell -> Tilemap -> Tilemap
removeTileInstantly cell tilemap =
    let
        ( tilemapWithoutTile, _ ) =
            removeTile cell tilemap

        tilemapUpdateResult =
            -- run a FSM update cycle to make sure that tiles are not transitioning
            updateTilemap (Duration.milliseconds 260) tilemapWithoutTile
    in
    tilemapUpdateResult.tilemap


type alias CellsByTileKind =
    { uninitialized : List CellCoordinates
    , fixed : List CellCoordinates
    , superposition : List CellCoordinates
    }


emptyCellsByTileKind : CellsByTileKind
emptyCellsByTileKind =
    { uninitialized = []
    , fixed = []
    , superposition = []
    }


cellsByTileKind : Tilemap -> CellsByTileKind
cellsByTileKind tilemap =
    let
        unsorted =
            foldTiles
                (\cell tile acc ->
                    let
                        coords =
                            Cell.coordinates cell
                    in
                    case tile.kind of
                        Tile.Unintialized ->
                            { acc | uninitialized = coords :: acc.uninitialized }

                        Tile.Fixed _ ->
                            { acc | fixed = coords :: acc.fixed }

                        Tile.Superposition _ ->
                            { acc | superposition = coords :: acc.superposition }
                )
                emptyCellsByTileKind
                tilemap
    in
    { uninitialized = List.sort unsorted.uninitialized
    , fixed = List.sort unsorted.fixed
    , superposition = List.sort unsorted.superposition
    }


{-| Lists CellCoordinates by tile kind based on the ascii art provided. Expects that the asciiArt has been stripped of whitespace.
-}
cellsByTileKindFromAscii : TilemapConfig -> String -> Result String CellsByTileKind
cellsByTileKindFromAscii tilemapConfig asciiArt =
    let
        processedRows =
            String.lines asciiArt
    in
    if List.length processedRows /= tilemapConfig.verticalCellsAmount then
        Err "Invalid rows amount"

    else
        processedRows
            |> List.indexedMap (\y -> processAsciiRow tilemapConfig (y + 1))
            |> List.foldl
                (\rowResult acc ->
                    Result.map2
                        (\okAcc okRow ->
                            { uninitialized = List.sort (okAcc.uninitialized ++ okRow.uninitialized)
                            , fixed = List.sort (okAcc.fixed ++ okRow.fixed)
                            , superposition = List.sort (okAcc.superposition ++ okRow.superposition)
                            }
                        )
                        rowResult
                        acc
                )
                (Ok emptyCellsByTileKind)


processAsciiRow : TilemapConfig -> Int -> String -> Result String CellsByTileKind
processAsciiRow tilemapConfig y_ row =
    let
        processedRow =
            String.toList row
    in
    if List.length processedRow /= tilemapConfig.horizontalCellsAmount then
        Err "Invalid columns amount"

    else
        processedRow
            |> List.indexedMap (\x char -> ( x + 1, y_, char ))
            |> List.foldl
                (\( x, y, char ) acc ->
                    case char of
                        '-' ->
                            { acc | uninitialized = ( x, y ) :: acc.uninitialized }

                        'x' ->
                            { acc | fixed = ( x, y ) :: acc.fixed }

                        'o' ->
                            { acc | superposition = ( x, y ) :: acc.superposition }

                        _ ->
                            acc
                )
                emptyCellsByTileKind
            |> Ok


tilemapToAscii : Tilemap -> String
tilemapToAscii tilemap =
    let
        cellsByTileKind_ =
            cellsByTileKind tilemap

        tilemapConfig =
            getTilemapConfig tilemap
    in
    List.concat
        [ List.map (Tuple.pair '-') cellsByTileKind_.uninitialized
        , List.map (Tuple.pair 'x') cellsByTileKind_.fixed
        , List.map (Tuple.pair 'o') cellsByTileKind_.superposition
        ]
        |> List.sortBy Tuple.second
        |> gridToAscii tilemapConfig.horizontalCellsAmount tilemapConfig.verticalCellsAmount


gridToAscii : Int -> Int -> List ( Char, CellCoordinates ) -> String
gridToAscii width height cells =
    let
        defaultGrid =
            List.repeat height (String.repeat width "-")

        updateGrid ( char, ( x, y ) ) grid =
            List.indexedMap
                (\rowIndex row ->
                    if rowIndex == y - 1 then
                        String.left (x - 1) row ++ String.fromChar char ++ String.dropLeft x row

                    else
                        row
                )
                grid

        finalGrid =
            List.foldl updateGrid defaultGrid cells
    in
    String.join "\n" finalGrid


createCell : TilemapConfig -> Int -> Int -> Cell
createCell constraints x y =
    Cell.fromCoordinatesUnsafe constraints ( x, y )


placeRoadAndUpdateBuffer : List CellCoordinates -> Tilemap -> Tilemap
placeRoadAndUpdateBuffer cellsToPlace tilemap =
    List.foldl
        (\( x, y ) nextTilemap ->
            let
                cell =
                    createCell (getTilemapConfig tilemap) x y
            in
            nextTilemap
                |> addTileInstantly cell
                |> updateBufferCells cell
        )
        tilemap
        cellsToPlace


removeRoadAndUpdateBuffer : CellCoordinates -> Tilemap -> Tilemap
removeRoadAndUpdateBuffer ( x, y ) tilemap =
    let
        cell =
            createCell (getTilemapConfig tilemap) x y
    in
    tilemap
        |> removeTileInstantly cell
        |> removeBuffer cell
