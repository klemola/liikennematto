module Tilemap.Update exposing (modifyTile, update)

import Audio exposing (playSound)
import Data.TileSet exposing (allTiles, nonRoadTiles, tileIdByBitmask, tilesByBaseTileId)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Debug exposing (DevAction(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        , withTilemap
        )
import Model.RenderCache exposing (refreshTilemapCache)
import Model.World as World
import Random
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , canBuildRoadAt
        , cellBitmask
        , cellHasFixedTile
        , fixedTileByCell
        , getTilemapConfig
        , mapTiles
        , resetSuperposition
        , setSuperpositionOptions
        , tileByCell
        , tileNeighborIn
        , updateTilemap
        )
import Tilemap.Tile as Tile exposing (Action(..), Tile, TileKind(..), isBuilt)
import Tilemap.TileConfig as TileConfig
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
                                        (resetSuperposition removedCell allTiles nextTilemap)
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

        TriggerDevAction action ->
            case action of
                RunWFC ->
                    runWFC model

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


onPrimaryInput : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
onPrimaryInput cell model =
    let
        alreadyExists =
            cellHasFixedTile cell model.world.tilemap
    in
    if not alreadyExists && canBuildRoadAt cell model.world.tilemap then
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


type alias ModifyTileConfig =
    { cell : Cell
    , tilemapChangeFn : Cell -> Tilemap -> ( Tilemap, List Action )
    , postModifyFn : Cell -> WFC.Model -> WFC.Model
    }


addTileNeighborInitDistance : Int
addTileNeighborInitDistance =
    2


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        bitmask =
            cellBitmask cell model.world.tilemap
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            modifyTileAndUpdate
                { cell = cell
                , tilemapChangeFn = Tilemap.Core.addTile tileId
                , postModifyFn = WFC.initializeArea addTileNeighborInitDistance nonRoadTiles
                }
                model

        Nothing ->
            ( model, Cmd.none )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    modifyTileAndUpdate
        { cell = cell
        , tilemapChangeFn = Tilemap.Core.removeTile
        , postModifyFn = \_ wfcModel -> wfcModel
        }
        model


modifyTileAndUpdate : ModifyTileConfig -> Liikennematto -> ( Liikennematto, Cmd Message )
modifyTileAndUpdate modifyTileConfig model =
    let
        { world } =
            model

        ( withWFC, actions ) =
            modifyTile modifyTileConfig world.tilemap world.seed
    in
    ( withTilemap withWFC model
    , Cmd.batch (tileActionsToCmds actions)
    )


modifyTile : ModifyTileConfig -> Tilemap -> Random.Seed -> ( Tilemap, List Action )
modifyTile { cell, tilemapChangeFn, postModifyFn } tilemap seed =
    let
        ( updatedTilemap, tilemapChangeActions ) =
            tilemapChangeFn cell tilemap

        wfcModel =
            WFC.fromTilemap updatedTilemap seed
                |> WFC.propagateConstraints cell

        ( updatedWfcModel, wfcTileActions ) =
            updateTileNeighbors cell wfcModel

        withInit =
            postModifyFn cell updatedWfcModel
    in
    ( WFC.toTilemap withInit
    , tilemapChangeActions ++ wfcTileActions
    )


updateTileNeighbors : Cell -> WFC.Model -> ( WFC.Model, List Action )
updateTileNeighbors cell wfcModel =
    updateTileNeighborsHelper
        cell
        OrthogonalDirection.all
        wfcModel
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
                Fixed _ ->
                    wfcModel
                        |> WFC.resetCell allTiles cell tile.kind
                        |> WFC.collapse cell

                Superposition _ ->
                    wfcModel

                Unintialized ->
                    WFC.resetCell nonRoadTiles cell tile.kind wfcModel

        Nothing ->
            wfcModel



--
-- Procgen
--


runWFC : Liikennematto -> ( Liikennematto, Cmd Message )
runWFC model =
    let
        wfc =
            if WFC.stopped model.wfc then
                WFC.fromTilemap (reopenRoads model.world.tilemap) model.world.seed

            else
                model.wfc

        ( updatedWfcModel, wfcTileActions ) =
            wfc
                |> WFC.stepN WFC.StopAtSolved 100
                |> WFC.flushPendingActions
    in
    ( withTilemap
        (WFC.toTilemap updatedWfcModel)
        model
    , Cmd.batch (tileActionsToCmds wfcTileActions)
    )


reopenRoads : Tilemap -> Tilemap
reopenRoads tilemap =
    mapTiles
        (\_ tile ->
            Tile.id tile
                |> Maybe.andThen
                    (\baseTileId ->
                        case tilesByBaseTileId baseTileId of
                            [] ->
                                Nothing

                            options ->
                                let
                                    tileVariations =
                                        -- TODO: filter by compatibility, e.g. check for tilemap edge and neighbor
                                        List.map TileConfig.tileConfigId options

                                    nextTile =
                                        { tile | kind = Superposition (baseTileId :: tileVariations) }
                                in
                                Just nextTile
                    )
                |> Maybe.withDefault tile
        )
        tilemap



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
