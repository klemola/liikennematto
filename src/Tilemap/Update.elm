module Tilemap.Update exposing (addTileById, update)

import Audio exposing (playSound)
import Data.TileSet
    exposing
        ( defaultTiles
        , nonRoadTiles
        , tileById
        , tileIdByBitmask
        , tilesByBaseTileId
        )
import Duration exposing (Duration)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import List.Nonempty
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Debug exposing (DevAction(..))
import Model.Liikennematto
    exposing
        ( DrivenWFC(..)
        , Liikennematto
        , drivenWfcInitialState
        , withTilemap
        )
import Model.RenderCache exposing (refreshTilemapCache)
import Model.World as World exposing (World)
import Quantity
import Random
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , cellBitmask
        , cellHasRoad
        , cellSupportsRoadPlacement
        , fixedTileByCell
        , foldTiles
        , getTilemapConfig
        , resetSuperposition
        , resetTileBySurroundings
        , setSuperpositionOptions
        , tileByCell
        , tileNeighborIn
        , updateTilemap
        )
import Tilemap.Tile as Tile exposing (Action(..), Tile, TileKind(..), isBuilt)
import Tilemap.TileConfig as TileConfig exposing (TileConfig, TileId)
import Tilemap.WFC as WFC
import UI.Core exposing (InputKind(..))


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        UpdateTilemap delta ->
            let
                { world, renderCache } =
                    model

                tilemapUpdateResult =
                    updateTilemap delta world.tilemap

                -- TODO: this is a hack around FSM actions not being able to include variable data
                withRemovedEffects =
                    { tilemapUpdateResult
                        | tilemap =
                            List.foldl
                                (\removedCell nextTilemap ->
                                    setSuperpositionOptions
                                        removedCell
                                        (resetSuperposition removedCell defaultTiles nextTilemap)
                                        nextTilemap
                                )
                                tilemapUpdateResult.tilemap
                                tilemapUpdateResult.emptiedCells
                    }

                ( nextWorld, maybeTilemapChange ) =
                    world
                        |> World.setTilemap withRemovedEffects.tilemap
                        |> World.resolveTilemapUpdate delta withRemovedEffects

                tilemapChangedEffects =
                    case maybeTilemapChange of
                        Just tilemapChange ->
                            Message.asCmd (TilemapChanged tilemapChange)

                        Nothing ->
                            Cmd.none

                ( nextRenderCache, dynamicTiles ) =
                    refreshTilemapCache withRemovedEffects renderCache
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
                , dynamicTiles = dynamicTiles
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds withRemovedEffects.actions)
            )

        GameSetupComplete ->
            let
                tilemapConfig =
                    getTilemapConfig model.world.tilemap
            in
            case
                Cell.fromCoordinates tilemapConfig
                    ( tilemapConfig.horizontalCellsAmount // 2
                    , tilemapConfig.verticalCellsAmount // 2
                    )
            of
                Just cell ->
                    addTile cell model

                Nothing ->
                    ( model, Cmd.none )

        InputReceived inputEvent ->
            case inputEvent.kind of
                Primary ->
                    onPrimaryInput inputEvent.cell model

                Secondary ->
                    onSecondaryInput inputEvent.cell model

        CheckQueues _ delta ->
            case model.world.pendingTilemapChange of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    runWFCIfNecessary model delta

        TriggerDevAction action ->
            case action of
                RunWFC ->
                    runWFCIfNecessary model (Duration.milliseconds 16)

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


onPrimaryInput : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
onPrimaryInput cell model =
    if not (cellHasRoad cell model.world.tilemap) && cellSupportsRoadPlacement cell model.world.tilemap then
        addTile cell model

    else
        ( model, Cmd.none )


onSecondaryInput : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
onSecondaryInput cell model =
    let
        tile =
            fixedTileByCell model.world.tilemap cell
    in
    if Maybe.unwrap False isBuilt tile then
        removeTile cell model

    else
        ( model, Cmd.none )



--
-- Tile modification
--


addTileNeighborInitDistance : Int
addTileNeighborInitDistance =
    3


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        { world } =
            model

        ( tilemapWithClearedCell, clearCellActions ) =
            Tilemap.Core.removeTile cell world.tilemap

        bitmask =
            cellBitmask cell tilemapWithClearedCell
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            let
                ( withWFC, addTileActions ) =
                    addTileById cell tileId tilemapWithClearedCell world.seed
            in
            ( withTilemap withWFC drivenWfcInitialState model
            , Cmd.batch (tileActionsToCmds (clearCellActions ++ addTileActions))
            )

        Nothing ->
            ( model, Cmd.none )


addTileById : Cell -> TileId -> Tilemap -> Random.Seed -> ( Tilemap, List Action )
addTileById cell tileId tilemap seed =
    let
        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.addTile tileId cell tilemap

        wfcModel =
            WFC.fromTilemap updatedTilemap seed
                |> WFC.propagateConstraints cell

        ( updatedWfcModel, wfcTileActions ) =
            updateTileNeighbors cell wfcModel

        withInit =
            WFC.initializeArea addTileNeighborInitDistance nonRoadTiles cell updatedWfcModel
    in
    ( WFC.toTilemap withInit
    , tilemapChangeActions ++ wfcTileActions
    )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    let
        { world } =
            model

        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.removeTile cell world.tilemap

        wfcModel =
            WFC.fromTilemap updatedTilemap world.seed
                |> WFC.propagateConstraints cell

        ( updatedWfcModel, wfcTileActions ) =
            updateTileNeighbors cell wfcModel

        ( withWFC, actions ) =
            ( WFC.toTilemap updatedWfcModel
            , tilemapChangeActions ++ wfcTileActions
            )
    in
    ( withTilemap withWFC drivenWfcInitialState model
    , Cmd.batch (tileActionsToCmds actions)
    )


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

                Superposition _ ->
                    wfcModel

                Unintialized ->
                    WFC.resetCell nonRoadTiles cell tile.kind wfcModel

        Nothing ->
            wfcModel



--
-- Procgen
--


runWFCIfNecessary : Liikennematto -> Duration -> ( Liikennematto, Cmd Message )
runWFCIfNecessary model delta =
    case model.wfc of
        WFCSolved ->
            ( model, Cmd.none )

        WFCPending timer ->
            let
                nextTimer =
                    timer
                        |> Quantity.minus delta
                        |> Quantity.max Quantity.zero
            in
            if Quantity.lessThanOrEqualToZero nextTimer then
                runWFC model (restartWFC model.world) False

            else
                ( { model | wfc = WFCPending nextTimer }, Cmd.none )

        WFCActive wfcModel ->
            case WFC.currentState wfcModel of
                WFC.Failed _ ->
                    runWFC model (restartWFC model.world) False

                WFC.Done ->
                    runWFC model wfcModel True

                -- Solving, Recovering
                _ ->
                    let
                        nextWfc =
                            WFC.stepN WFC.StopAtSolved 100 wfcModel
                    in
                    runWFC model nextWfc False


runWFC : Liikennematto -> WFC.Model -> Bool -> ( Liikennematto, Cmd Message )
runWFC model wfc isSolved =
    let
        ( updatedWfcModel, wfcTileActions ) =
            if isSolved then
                WFC.flushPendingActions wfc

            else
                ( wfc, [] )

        ( nextTilemap, nextDrivenWfc ) =
            if isSolved then
                ( WFC.toTilemap updatedWfcModel, WFCSolved )

            else
                ( model.world.tilemap, WFCActive updatedWfcModel )
    in
    ( withTilemap
        nextTilemap
        nextDrivenWfc
        model
    , Cmd.batch (tileActionsToCmds wfcTileActions)
    )


restartWFC : World -> WFC.Model
restartWFC world =
    WFC.fromTilemap
        (reopenRoads world.tilemap)
        world.seed


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
                                        Fixed _ ->
                                            Nothing

                                        _ ->
                                            Just neighborCell
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



--
-- Helpers
--


tileActionsToCmds : List Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                PlayAudio sound ->
                    playSound sound

                Tile.OnRemoved _ ->
                    Cmd.none
        )
