module Tilemap.DrivenWFC exposing
    ( DrivenWFC(..)
    , drivenWfcInitialState
    , resetWFC
    , restartWFC
    , runWFC
    , updateTileNeighbors
    )

import Data.TileSet
    exposing
        ( decorativeTiles
        , defaultTiles
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
    | WFCSolved


minWfcUpdateFrequency : Duration
minWfcUpdateFrequency =
    Duration.milliseconds 250


drivenWfcInitialState : DrivenWFC
drivenWfcInitialState =
    WFCPending minWfcUpdateFrequency


wfcStepsPerCycle : Int
wfcStepsPerCycle =
    100


runWFC : Random.Seed -> Tilemap -> WFC.Model -> ( Tilemap, DrivenWFC, List Tile.Action )
runWFC seed tilemap wfc =
    let
        ( baseWFC, isSolved ) =
            case WFC.currentState wfc of
                WFC.Failed _ ->
                    ( restartWFC seed (WFC.toTileInventory wfc) tilemap, False )

                WFC.Done ->
                    ( wfc, True )

                -- Solving, Recovering
                _ ->
                    ( wfc, False )
    in
    if isSolved then
        let
            ( solvedWfc, tileActions ) =
                WFC.flushPendingActions baseWFC
        in
        ( WFC.toTilemap solvedWfc
        , WFCSolved
        , tileActions
        )

    else
        let
            nextWfc =
                WFC.stepN
                    WFC.StopAtSolved
                    wfcStepsPerCycle
                    baseWFC
        in
        ( tilemap, WFCActive nextWfc, [] )


restartWFC : Random.Seed -> TileInventory Int -> Tilemap -> WFC.Model
restartWFC seed tileInventory tilemap =
    resetWFC seed
        Nothing
        tileInventory
        (reopenRoads tilemap)


resetWFC : Random.Seed -> Maybe Cell -> TileInventory Int -> Tilemap -> WFC.Model
resetWFC seed changedCell tileInventory tilemap =
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
                            |> WFC.resetCell defaultTiles cell tile.kind
                            |> WFC.collapse cell

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
                -- TODO: this fake tile definition is wrong, maybe make the fn use different param
                (Superposition [])
                nextTilemap
        )
        tilemap
        drivewayNeighbors
