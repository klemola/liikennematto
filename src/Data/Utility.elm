module Data.Utility exposing
    ( CellsByTileKind
    , addLotByEntryCell
    , cellsByTileKind
    , cellsByTileKindFromAscii
    , createCell
    , getStartAndEndNode
    , initTileWithSuperposition
    , multilineGridDebug
    , placeRoadAndUpdateBuffer
    , removeRoadAndUpdateBuffer
    , tenByTenTilemap
    , testSeed
    , tilemapFromCoordinates
    , tilemapToAscii
    , worldFromTilemap
    )

import Data.Lots exposing (NewLot)
import Data.TileSet
    exposing
        ( horizontalRoadLotEntryUp
        , lotTiles
        , tileIdByBitmask
        , tileIdsByOrthogonalMatch
        , verticalRoadLotEntryLeft
        , verticalRoadLotEntryRight
        )
import Duration
import Lib.OrthogonalDirection as OrthogonalDirection
import Model.World as World exposing (World, newLotMatchesTile)
import Random
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Simulation.Update exposing (addLot)
import Tilemap.Buffer exposing (removeBuffer, updateBufferCells)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addTileFromWfc
        , cellBitmask
        , createTilemap
        , foldTiles
        , getTilemapConfig
        , mapCell
        , removeTile
        , updateTilemap
        )
import Tilemap.DrivenWFC exposing (addTileById)
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig as TileConfig exposing (TileConfig)
import Tilemap.WFC as WFC


testSeed : Random.Seed
testSeed =
    Random.initialSeed 42


tenByTenTilemap : TilemapConfig
tenByTenTilemap =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


tilemapFromCoordinates : TilemapConfig -> List CellCoordinates -> Tilemap
tilemapFromCoordinates tilemapConfig cellCoordinates =
    let
        cells =
            List.filterMap (Cell.fromCoordinates tenByTenTilemap) cellCoordinates
    in
    tilemapFromCells tilemapConfig cells


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty testSeed (getTilemapConfig tilemap)
        |> World.setTilemap tilemap


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
                    let
                        world =
                            worldFromTilemap tilemap
                    in
                    addTileById world.seedState (World.tileInventoryCount world) cell tileId tilemap

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
    , buffer : List CellCoordinates
    }


emptyCellsByTileKind : CellsByTileKind
emptyCellsByTileKind =
    { uninitialized = []
    , fixed = []
    , superposition = []
    , buffer = []
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

                        Tile.Buffer ->
                            { acc | buffer = coords :: acc.buffer }
                )
                emptyCellsByTileKind
                tilemap
    in
    { uninitialized = List.sort unsorted.uninitialized
    , fixed = List.sort unsorted.fixed
    , superposition = List.sort unsorted.superposition
    , buffer = List.sort unsorted.buffer
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
                            , buffer = List.sort (okAcc.buffer ++ okRow.buffer)
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

                        'b' ->
                            { acc | buffer = ( x, y ) :: acc.buffer }

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
        , List.map (Tuple.pair 'b') cellsByTileKind_.buffer
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


multilineGridDebug : String -> String -> String
multilineGridDebug label str =
    str
        |> String.lines
        |> List.indexedMap
            (\i line ->
                let
                    lineNumber =
                        String.fromInt (i + 1) |> String.pad 2 ' '
                in
                String.join " " [ label, "line", lineNumber, line ]
            )
        |> String.join "\n"


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



--
-- WFC helpers
--


initTileWithSuperposition : TilemapConfig -> List TileConfig -> Int -> Tile
initTileWithSuperposition constraints tileSet index =
    index
        |> Cell.fromArray1DIndexUnsafe constraints
        |> Cell.connectedBounds constraints
        |> tileIdsByOrthogonalMatch tileSet
        |> Tile.Superposition
        |> Tile.init



--
-- Lots
--


addLotByEntryCell : Cell -> NewLot -> World -> Result String World
addLotByEntryCell lotEntryCell newLot world =
    let
        { tilemap } =
            world

        tilemapConfig =
            getTilemapConfig tilemap

        drivewayCellMatch =
            Cell.nextOrthogonalCell tilemapConfig newLot.entryDirection lotEntryCell

        largeTileMatch =
            lotTiles
                |> List.filterMap
                    (\tileConfig ->
                        case tileConfig of
                            TileConfig.Large largeTile ->
                                if newLotMatchesTile largeTile newLot then
                                    Just largeTile

                                else
                                    Nothing

                            TileConfig.Single _ ->
                                Nothing
                    )
                |> List.head

        lotEntry =
            lotEntryTile lotEntryCell newLot tilemap
                |> Maybe.withDefault (Tile.init Tile.Buffer)

        tilemapWithLotEntry =
            mapCell lotEntryCell (\_ -> lotEntry) tilemap

        attemptBuildLot drivewayCell largeTile =
            WFC.checkLargeTileFit tilemapWithLotEntry drivewayCell largeTile
                |> Result.fromMaybe "Invalid lot placement: large tile does not fit"
                |> Result.andThen (\_ -> WFC.largeTileSubgrid tilemapWithLotEntry drivewayCell largeTile)
                |> Result.map
                    (\largeTileCells ->
                        let
                            tilemapWithLargeTile =
                                List.foldl
                                    (\( cell, subgridProps ) nextTilemap ->
                                        addTileFromWfc
                                            (Just ( subgridProps.parentTileId, subgridProps.index ))
                                            (TileConfig.Single subgridProps.singleTile)
                                            cell
                                            nextTilemap
                                            |> Tuple.first
                                    )
                                    tilemapWithLotEntry
                                    largeTileCells
                        in
                        world
                            |> World.setTilemap tilemapWithLargeTile
                            |> addLot (\_ worldWithLot -> worldWithLot) largeTile newLot drivewayCell
                            |> (\( worldWithLot, lotPlacement ) ->
                                    World.setTilemap (tilemapWithLotPlacement lotPlacement worldWithLot.tilemap) worldWithLot
                               )
                            |> World.updateRoadNetwork
                    )
    in
    case ( drivewayCellMatch, largeTileMatch ) of
        ( Just drivewayCell, Just largeTile ) ->
            attemptBuildLot drivewayCell largeTile

        _ ->
            Err "Invalid lot placement: invalid driveway or no matching large tile"


lotEntryTile : Cell -> NewLot -> Tilemap -> Maybe Tile
lotEntryTile cell newLot tilemap =
    let
        lotEntryTileConfig =
            case ( cellBitmask cell tilemap, newLot.entryDirection ) of
                ( 9, OrthogonalDirection.Right ) ->
                    Just verticalRoadLotEntryRight

                ( 9, OrthogonalDirection.Left ) ->
                    Just verticalRoadLotEntryLeft

                ( 6, OrthogonalDirection.Up ) ->
                    Just horizontalRoadLotEntryUp

                _ ->
                    Nothing
    in
    lotEntryTileConfig
        |> Maybe.map
            (\tileConfig ->
                Tile.fromTileConfig tileConfig Nothing Tile.AddFromWFC
            )
        |> Maybe.map Tuple.first


tilemapWithLotPlacement : World.LotPlacement -> Tilemap -> Tilemap
tilemapWithLotPlacement { lot, tile, drivewayCell } tilemap =
    let
        constraints =
            getTilemapConfig tilemap

        topLeftCell =
            Tile.largeTileTopLeftCell constraints drivewayCell tile.anchorIndex tile
                -- This fallback is unnecessary - the cell should exist
                |> Maybe.withDefault drivewayCell
    in
    mapCell topLeftCell
        (Tile.withName lot.name)
        tilemap
