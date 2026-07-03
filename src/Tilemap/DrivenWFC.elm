module Tilemap.DrivenWFC exposing
    ( DrivenWFC(..)
    , RunWFCResult
    , addTileById
    , bufferToSuperposition
    , drivenWfcDebug
    , initDrivenWfc
    , onRemoveTile
    , pruneUnfittableLargeTiles
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
import Dict
import Duration exposing (Duration)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Lib.SeedState exposing (SeedState)
import List.Nonempty
import Round
import Set
import Tilemap.Buffer
    exposing
        ( applyRevertFromDict
        , registerPlacement
        , removeBuffer
        )
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , addTileFromWfc
        , clearSavedNature
        , foldTiles
        , getSavedNatureAnchors
        , getTilemapConfig
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
    | WFCActive Int WFC.Model
    | WFCFailed (List String) SeedState
    | WFCSolved Int (List String) (List ( Cell, TileId )) SeedState


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


runWfc : Int -> Tilemap -> WFC.Model -> RunWFCResult
runWfc runId tilemap wfc =
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
                runId
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
            ( tilemap, WFCActive runId nextWfc, [] )


{-| The revert is applied here rather than at call sites so that every restart
(initial run and failure retry both) forks the same view of the trail area.
-}
restartWfc : SeedState -> TileInventory Int -> Tilemap -> WFC.Model
restartWfc seedState tileInventory tilemap =
    resetWfc seedState
        Nothing
        tileInventory
        (preprocessTilemap (applyRevertFromDict tilemap))


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

        updatedWfcModel =
            resetWfc seedState (Just cell) tileInventory (updateTileNeighbors cell updatedTilemap)

        placed =
            WFC.toTilemap updatedWfcModel
    in
    ( registerPlacement cell placed
    , tilemapChangeActions
    )


onRemoveTile : SeedState -> TileInventory Int -> Cell -> Tilemap -> ( WFC.Model, List Action )
onRemoveTile seedState tileInventory cell tilemap =
    let
        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.removeTile cell tilemap

        withBufferRemoved =
            updatedTilemap
                |> removeBuffer cell
                |> clearSavedNature

        wfcWithoutTile =
            resetWfc seedState (Just cell) tileInventory (updateTileNeighbors cell withBufferRemoved)
    in
    ( wfcWithoutTile, tilemapChangeActions )



--
-- Update tile neighbors after a tile has changed (by user input)
--


updateTileNeighbors : Cell -> Tilemap -> Tilemap
updateTileNeighbors cell tilemap =
    updateTileNeighborsHelper cell OrthogonalDirection.all tilemap


updateTileNeighborsHelper : Cell -> List OrthogonalDirection -> Tilemap -> Tilemap
updateTileNeighborsHelper origin remainingDirs tilemap =
    case remainingDirs of
        [] ->
            tilemap

        dir :: otherDirs ->
            let
                maybeTile =
                    tileNeighborIn dir origin tileByCell tilemap

                nextTilemap =
                    processTileNeighbor maybeTile tilemap
            in
            updateTileNeighborsHelper origin otherDirs nextTilemap


processTileNeighbor : Maybe ( Cell, Tile ) -> Tilemap -> Tilemap
processTileNeighbor maybeTile tilemap =
    case maybeTile of
        Just ( cell, tile ) ->
            case tile.kind of
                Fixed properties ->
                    if TileConfig.biome (tileById properties.id) == TileConfig.Road then
                        tilemap
                            |> resetFixedTileBySurroundings cell
                            |> collapseNeighborDirectly cell

                    else
                        tilemap

                -- Does not change Superposition, Buffer or Uninitialized
                _ ->
                    tilemap

        Nothing ->
            tilemap


collapseNeighborDirectly : Cell -> Tilemap -> Tilemap
collapseNeighborDirectly cell tilemap =
    case tileByCell tilemap cell |> Maybe.map .kind of
        -- Room for improvement: if cosmetic road variations are added, this should be a random pick
        Just (Superposition (first :: _)) ->
            addTileFromWfc Nothing (tileById first) cell tilemap
                |> Tuple.first

        _ ->
            tilemap



--
-- Preprocess tilemap for a WFC pass
--


type alias DrivewayNeighborProperties =
    ( Cell, List TileConfig )


preprocessTilemap : Tilemap -> Tilemap
preprocessTilemap tilemap =
    tilemap
        |> bufferToSuperposition
        |> pruneUnfittableLargeTiles
        |> reopenRoads
        |> restrictReclaimedFootprints


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


pruneUnfittableLargeTiles : Tilemap -> Tilemap
pruneUnfittableLargeTiles tilemap =
    foldTiles
        (\cell tile nextTilemap ->
            case tile.kind of
                Superposition options ->
                    let
                        remaining =
                            List.filter (largeTileFitsAt nextTilemap cell) options
                    in
                    if List.length remaining == List.length options then
                        nextTilemap

                    else
                        setSuperpositionOptions cell remaining nextTilemap

                _ ->
                    nextTilemap
        )
        tilemap
        tilemap


largeTileFitsAt : Tilemap -> Cell -> TileId -> Bool
largeTileFitsAt tilemap cell tileId =
    case tileById tileId of
        TileConfig.Large largeTile ->
            WFC.checkLargeTileFit tilemap cell largeTile /= Nothing

        TileConfig.Single _ ->
            True


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


{-| The large tile fit check only requires covered cells to be in
superposition, so the filter extends one cell beyond the footprint — otherwise
a neighboring anchor could span back into it. The margin needs to grow if
nature tiles larger than 1x2 are added. Runs last so that lot options from
reopened roads survive.
-}
restrictReclaimedFootprints : Tilemap -> Tilemap
restrictReclaimedFootprints tilemap =
    Dict.foldl restrictFootprintAndMargin tilemap (getSavedNatureAnchors tilemap)


restrictFootprintAndMargin : CellCoordinates -> TileId -> Tilemap -> Tilemap
restrictFootprintAndMargin topLeftCoords largeTileId tilemap =
    case ( Cell.fromCoordinates (getTilemapConfig tilemap) topLeftCoords, tileById largeTileId ) of
        ( Just topLeftCell, TileConfig.Large largeTile ) ->
            let
                constraints =
                    getTilemapConfig tilemap

                footprint =
                    Tile.largeTileCells constraints topLeftCell largeTile

                margin =
                    footprint
                        |> List.concatMap
                            (Cell.orthogonalNeighbors constraints >> List.map Tuple.second)
            in
            List.foldl restrictCellOptions tilemap (footprint ++ margin)

        _ ->
            tilemap


restrictCellOptions : Cell -> Tilemap -> Tilemap
restrictCellOptions cell tilemap =
    case tileByCell tilemap cell |> Maybe.map .kind of
        Just (Superposition options) ->
            setSuperpositionOptions cell (List.filter (not << isNatureLargeTile) options) tilemap

        _ ->
            tilemap


isNatureLargeTile : TileId -> Bool
isNatureLargeTile tileId =
    case tileById tileId of
        TileConfig.Large largeTile ->
            largeTile.biome == TileConfig.Nature

        TileConfig.Single _ ->
            False


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

        WFCActive _ _ ->
            "Active"

        WFCFailed _ _ ->
            "Failed"

        WFCSolved _ _ _ _ ->
            "Solved"
