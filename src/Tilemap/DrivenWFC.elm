module Tilemap.DrivenWFC exposing
    ( DrivenWFC(..)
    , RunWFCResult
    , drivenWfcDebug
    , initDrivenWfc
    , resetWfc
    , restartWfc
    , runWfc
    , updateTileNeighbors
    )

import Data.TileSet
    exposing
        ( decorativeTiles
        , lotTiles
        , tileById
        , tilesByBaseTileId
        )
import Duration exposing (Duration)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import List.Nonempty
import Random
import Tilemap.Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , foldTiles
        , resetFixedTileBySurroundings
        , resetTileBySurroundings
        , setSuperpositionOptions
        , tileByCell
        , tileNeighborIn
        )
import Tilemap.Tile as Tile exposing (Action(..), Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId)
import Tilemap.TileInventory exposing (TileInventory)
import Tilemap.WFC as WFC


type DrivenWFC
    = WFCPending Duration
    | WFCActive WFC.Model
    | WFCFailed (List String) Random.Seed
    | WFCSolved (List String)


type alias RunWFCResult =
    ( Tilemap, DrivenWFC, List Tile.Action )


minWfcUpdateFrequency : Duration
minWfcUpdateFrequency =
    Duration.milliseconds 250


initDrivenWfc : DrivenWFC
initDrivenWfc =
    WFCPending minWfcUpdateFrequency


wfcStepsPerCycle : Int
wfcStepsPerCycle =
    3500


runWfc : Tilemap -> WFC.Model -> RunWFCResult
runWfc tilemap wfc =
    case WFC.currentState wfc of
        WFC.Failed _ ->
            ( tilemap
            , WFCFailed
                (WFC.log wfc)
                (WFC.currentSeed wfc)
            , []
            )

        WFC.Done ->
            let
                ( solvedWfc, tileActions ) =
                    WFC.flushPendingActions wfc
            in
            ( WFC.toTilemap solvedWfc
            , WFCSolved (WFC.log solvedWfc)
            , tileActions
            )

        -- Solving, Recovering
        _ ->
            let
                nextWfc =
                    WFC.stepN
                        WFC.StopAtSolved
                        wfcStepsPerCycle
                        wfc
            in
            ( tilemap, WFCActive nextWfc, [] )


restartWfc : Random.Seed -> TileInventory Int -> Tilemap -> WFC.Model
restartWfc seed tileInventory tilemap =
    resetWfc seed
        Nothing
        tileInventory
        (reopenRoads tilemap)


resetWfc : Random.Seed -> Maybe Cell -> TileInventory Int -> Tilemap -> WFC.Model
resetWfc seed changedCell tileInventory tilemap =
    let
        wfcWithChangedTile =
            WFC.fromTilemap tilemap seed
                |> WFC.withTileInventory tileInventory
    in
    case changedCell of
        Just cell ->
            WFC.propagateConstraints cell wfcWithChangedTile

        Nothing ->
            wfcWithChangedTile



--
-- Update tile neighbors after a tile has changed (by user input)
--


updateTileNeighbors : Cell -> WFC.Model -> ( WFC.Model, List Action )
updateTileNeighbors cell wfcModel =
    wfcModel
        |> updateTileNeighborsHelper cell OrthogonalDirection.all
        |> WFC.flushPendingActions


updateTileNeighborsHelper : Cell -> List OrthogonalDirection -> WFC.Model -> WFC.Model
updateTileNeighborsHelper origin remainingDirs wfcModel =
    case remainingDirs of
        [] ->
            wfcModel

        dir :: otherDirs ->
            let
                maybeTile =
                    tileNeighborIn dir origin tileByCell (WFC.toTilemap wfcModel)

                nextWFCModel =
                    processTileNeighbor maybeTile wfcModel
            in
            updateTileNeighborsHelper origin otherDirs nextWFCModel


processTileNeighbor : Maybe ( Cell, Tile ) -> WFC.Model -> WFC.Model
processTileNeighbor maybeTile wfcModel =
    case maybeTile of
        Just ( cell, tile ) ->
            case tile.kind of
                Fixed properties ->
                    if TileConfig.biome (tileById properties.id) == TileConfig.Road then
                        wfcModel
                            |> WFC.withTilemapUpdate
                                (\wfcTilemap ->
                                    resetFixedTileBySurroundings cell wfcTilemap
                                )
                            |> WFC.collapse cell
                            |> Tuple.first

                    else
                        wfcModel

                -- Does not change Superposition or Uninitialized
                _ ->
                    wfcModel

        Nothing ->
            wfcModel



--
-- Reopen road tiles for a WFC pass
--


type alias DrivewayNeighborProperties =
    ( Cell, List TileConfig )


reopenRoads : Tilemap -> Tilemap
reopenRoads tilemap =
    foldTiles
        (\cell tile nextTilemap ->
            case Tile.id tile of
                Just baseTileId ->
                    reopenRoadsStep baseTileId cell nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        tilemap


reopenRoadsStep : TileId -> Cell -> Tilemap -> Tilemap
reopenRoadsStep baseTileId origin tilemap =
    case tilesByBaseTileId baseTileId of
        [] ->
            tilemap

        options ->
            let
                ( tileVariations, drivewayNeighbors ) =
                    findRoadVariations origin options tilemap
            in
            case tileVariations of
                [] ->
                    -- No valid variations, keep the tile fixed
                    tilemap

                variations ->
                    tilemap
                        |> setSuperpositionOptions origin (baseTileId :: variations)
                        |> resetDrivewayNeighbors drivewayNeighbors


findRoadVariations : Cell -> List TileConfig -> Tilemap -> ( List TileConfig.TileId, List DrivewayNeighborProperties )
findRoadVariations origin options tilemap =
    List.foldl
        (\option (( filteredOptionsAcc, drivewayNeighborsAcc ) as acc) ->
            let
                tileConfigId =
                    TileConfig.tileConfigId option

                sockets =
                    TileConfig.socketsList option

                -- The origin needs to be "open" (in superposition) so that the large tile fit check
                -- can simulate actual WFC steps
                tilemapWithOriginOpened =
                    setSuperpositionOptions origin [ tileConfigId ] tilemap

                toDrivewayNeighbor ( dir, socket ) =
                    if socket == Data.TileSet.lotEntrySocket then
                        tilemap
                            |> tileNeighborIn dir origin tileByCell
                            |> Maybe.andThen (findValidLotTiles tilemapWithOriginOpened)

                    else
                        Nothing

                drivewayNeighbors =
                    sockets
                        |> List.Nonempty.toList
                        |> List.filterMap toDrivewayNeighbor
            in
            if List.length drivewayNeighbors > 0 then
                ( tileConfigId :: filteredOptionsAcc
                , drivewayNeighborsAcc ++ drivewayNeighbors
                )

            else
                acc
        )
        ( [], [] )
        options


findValidLotTiles : Tilemap -> ( Cell, Tile ) -> Maybe DrivewayNeighborProperties
findValidLotTiles tilemapWithOpenOrigin ( neighborCell, neighborTile ) =
    case neighborTile.kind of
        Superposition _ ->
            let
                validLotTiles =
                    List.filterMap
                        (\lotTile ->
                            case lotTile of
                                TileConfig.Large largeTile ->
                                    WFC.checkLargeTileFit tilemapWithOpenOrigin neighborCell largeTile
                                        |> Maybe.map (\_ -> lotTile)

                                TileConfig.Single _ ->
                                    Nothing
                        )
                        lotTiles
            in
            if List.isEmpty validLotTiles then
                Nothing

            else
                Just ( neighborCell, validLotTiles )

        _ ->
            Nothing


resetDrivewayNeighbors : List ( Cell, List TileConfig ) -> Tilemap -> Tilemap
resetDrivewayNeighbors drivewayNeighbors tilemap =
    List.foldl
        (\( neighbor, validLotTiles ) nextTilemap ->
            resetTileBySurroundings neighbor
                (decorativeTiles ++ validLotTiles)
                nextTilemap
        )
        tilemap
        drivewayNeighbors


drivenWfcDebug : DrivenWFC -> String
drivenWfcDebug drivenWfc =
    case drivenWfc of
        WFCPending dur ->
            "Pending " ++ (dur |> Duration.inMilliseconds |> String.fromFloat)

        WFCActive _ ->
            "Active"

        WFCFailed _ _ ->
            "Failed"

        WFCSolved _ ->
            "Solved"
