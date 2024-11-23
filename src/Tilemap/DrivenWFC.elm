module Tilemap.DrivenWFC exposing (DrivenWFC(..), restartWFC, runWFC, toWfcModel)

import Data.TileSet
    exposing
        ( nonRoadTiles
        , tilesByBaseTileId
        )
import Duration exposing (Duration)
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
import Tilemap.Tile as Tile exposing (TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId)
import Tilemap.WFC as WFC


type DrivenWFC
    = WFCPending Duration
    | WFCActive WFC.Model
    | WFCSolved


wfcStepsPerCycle : Int
wfcStepsPerCycle =
    1000


runWFC : Random.Seed -> Tilemap -> WFC.Model -> ( Tilemap, DrivenWFC, List Tile.Action )
runWFC seed tilemap wfc =
    let
        ( baseWFC, isSolved ) =
            case WFC.currentState wfc of
                WFC.Failed _ ->
                    ( restartWFC seed tilemap, False )

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


restartWFC : Random.Seed -> Tilemap -> WFC.Model
restartWFC seed tilemap =
    WFC.fromTilemap
        (reopenRoads tilemap)
        seed


toWfcModel : DrivenWFC -> Maybe WFC.Model
toWfcModel drivenWfc =
    case drivenWfc of
        WFCActive wfc ->
            Just wfc

        _ ->
            Nothing



--
-- Reopen road tiles for a WFC pass
--


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
                    findVariations origin options tilemap
            in
            case tileVariations of
                [] ->
                    -- No valid variations, keep the tile fixed
                    tilemap

                variations ->
                    tilemap
                        |> setSuperpositionOptions origin (baseTileId :: variations)
                        |> resetDrivewayNeighbors drivewayNeighbors


findVariations : Cell -> List TileConfig -> Tilemap -> ( List TileConfig.TileId, List Cell )
findVariations origin options tilemap =
    List.foldl
        (\option (( filteredOptionsAcc, drivewayNeighborsAcc ) as acc) ->
            let
                tileConfigId =
                    TileConfig.tileConfigId option

                sockets =
                    TileConfig.socketsList option

                toDrivewayNeighbor ( dir, socket ) =
                    if socket == Data.TileSet.lotEntrySocket then
                        tilemap
                            |> tileNeighborIn dir origin tileByCell
                            |> Maybe.andThen
                                (\( neighborCell, neighborTile ) ->
                                    case neighborTile.kind of
                                        Superposition _ ->
                                            Just neighborCell

                                        _ ->
                                            Nothing
                                )

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


resetDrivewayNeighbors : List Cell -> Tilemap -> Tilemap
resetDrivewayNeighbors drivewayNeighbors tilemap =
    List.foldl
        (\neighbor nextTilemap ->
            resetTileBySurroundings neighbor
                nonRoadTiles
                -- TODO: this fake tile definition is wrong, maybe make the fn use different param
                (Superposition [])
                nextTilemap
        )
        tilemap
        drivewayNeighbors
