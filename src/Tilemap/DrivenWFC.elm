module Tilemap.DrivenWFC exposing
    ( DrivenWFC(..)
    , RunWFCResult
    , addTileById
    , bufferToSuperposition
    , drivenWfcDebug
    , initDrivenWfc
    , onRemoveTile
    , restartWfc
    , runWfc
    )

import Data.TileSet
    exposing
        ( basicRoadTiles
        , decorativeTiles
        , lotTiles
        , tileById
        , tilesByBaseTileId
        )
import Duration exposing (Duration)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Lib.SeedState exposing (SeedState)
import List.Nonempty
import Random
import Round
import Set
import Tilemap.Buffer exposing (removeBuffer, updateBufferCells)
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
import Tilemap.Tile as Tile exposing (Action, Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId)
import Tilemap.TileInventory exposing (TileInventory)
import Tilemap.WFC as WFC
import Time


type DrivenWFC
    = WFCPending Duration Time.Posix
    | WFCActive WFC.Model
    | WFCFailed (List String) SeedState
    | WFCSolved (List String) (List ( Cell, TileId )) SeedState


type alias RunWFCResult =
    ( Tilemap, DrivenWFC, List Tile.Action )


minWfcUpdateFrequency : Duration
minWfcUpdateFrequency =
    Duration.milliseconds 250


initDrivenWfc : Time.Posix -> DrivenWFC
initDrivenWfc time =
    WFCPending minWfcUpdateFrequency time


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
            , WFCSolved
                (WFC.log solvedWfc)
                (WFC.collapsedTiles solvedWfc)
                (WFC.currentSeed solvedWfc)
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


restartWfc : SeedState -> TileInventory Int -> Tilemap -> WFC.Model
restartWfc seedState tileInventory tilemap =
    resetWfc seedState
        Nothing
        tileInventory
        (preprocessTilemap tilemap)


resetWfc : SeedState -> Maybe Cell -> TileInventory Int -> Tilemap -> WFC.Model
resetWfc seedState changedCell tileInventory tilemap =
    let
        wfcWithChangedTile =
            WFC.fromTilemap tilemap seedState
                |> WFC.withTileInventory tileInventory
    in
    case changedCell of
        Just cell ->
            WFC.propagateConstraints cell wfcWithChangedTile

        Nothing ->
            wfcWithChangedTile


addTileById : SeedState -> TileInventory Int -> Cell -> TileId -> Tilemap -> ( Tilemap, List Action )
addTileById seedState tileInventory cell tileId tilemap =
    let
        tileConfig =
            tileById tileId

        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.addTile tileConfig cell tilemap

        wfcModel =
            resetWfc seedState (Just cell) tileInventory updatedTilemap

        ( updatedWfcModel, wfcTileActions ) =
            updateTileNeighbors cell wfcModel
    in
    ( updateBufferCells cell (WFC.toTilemap updatedWfcModel)
    , tilemapChangeActions ++ wfcTileActions
    )


onRemoveTile : SeedState -> TileInventory Int -> Cell -> Tilemap -> ( WFC.Model, List Action )
onRemoveTile seedState tileInventory cell tilemap =
    let
        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.removeTile cell tilemap

        withBufferRemoved =
            removeBuffer cell updatedTilemap

        wfcModel =
            resetWfc seedState (Just cell) tileInventory withBufferRemoved

        ( wfcWithoutTile, wfcActions ) =
            updateTileNeighbors cell wfcModel
    in
    ( wfcWithoutTile, tilemapChangeActions ++ wfcActions )



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

                -- Does not change Superposition, Buffer or Uninitialized
                _ ->
                    wfcModel

        Nothing ->
            wfcModel



--
-- Preprocess tilemap for a WFC pass
--


type alias DrivewayNeighborProperties =
    ( Cell, List TileConfig )


preprocessTilemap : Tilemap -> Tilemap
preprocessTilemap tilemap =
    tilemap
        |> bufferToSuperposition
        |> reopenRoads


bufferToSuperposition : Tilemap -> Tilemap
bufferToSuperposition tilemap =
    foldTiles
        (\cell tile nextTilemap ->
            case tile.kind of
                Buffer ->
                    resetTileBySurroundings cell decorativeTiles nextTilemap

                _ ->
                    nextTilemap
        )
        tilemap
        tilemap


reopenRoads : Tilemap -> Tilemap
reopenRoads tilemap =
    foldTiles
        (\cell tile nextTilemap ->
            case tile.kind of
                Fixed properties ->
                    if Set.member properties.id basicRoadTiles then
                        reopenRoadsStep properties.id cell nextTilemap

                    else
                        nextTilemap

                _ ->
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


drivenWfcDebug : Time.Posix -> DrivenWFC -> String
drivenWfcDebug currentTime drivenWfc =
    case drivenWfc of
        WFCPending dur initTime ->
            String.join " "
                [ "Pending"
                , dur
                    |> Duration.inMilliseconds
                    |> Round.round 2
                , String.fromInt
                    ((Time.posixToMillis currentTime - Time.posixToMillis initTime) // 1000)
                ]

        WFCActive _ ->
            "Active"

        WFCFailed _ _ ->
            "Failed"

        WFCSolved _ _ _ ->
            "Solved"
