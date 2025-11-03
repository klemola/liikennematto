module Tilemap.Update exposing
    ( onPrimaryInput
    , onSecondaryInput
    , update
    )

import Audio exposing (playSound)
import Data.TileSet
    exposing
        ( decorativeTiles
        , tileIdByBitmask
        )
import Duration
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Debug exposing (appendWfcLog)
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (refreshTilemapCache, setTilemapCache, setTilemapDebugCache)
import Model.World as World exposing (LotPlacement, World)
import Process
import Quantity
import Savegame
import Task
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , addAnimationTimer
        , cellBitmask
        , cellSupportsRoadPlacement
        , extractRoadTile
        , fixedTileByCell
        , getBuildHistory
        , getTilemapConfig
        , mapCell
        , resetSuperposition
        , roadTileFromCell
        , setSuperpositionOptions
        , updateTilemap
        )
import Tilemap.DrivenWFC
    exposing
        ( DrivenWFC(..)
        , addTileById
        , initDrivenWfc
        , onRemoveTile
        , restartWfc
        , runWfc
        )
import Tilemap.Tile as Tile
    exposing
        ( Action(..)
        , isBuilt
        )
import Tilemap.WFC as WFC
import Time


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
                                        (resetSuperposition removedCell decorativeTiles nextTilemap)
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

                nextRenderCache =
                    refreshTilemapCache withRemovedEffects renderCache
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
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

        CheckQueues time delta ->
            case model.world.pendingTilemapChange of
                Just _ ->
                    ( model, Cmd.none )

                Nothing ->
                    case model.wfc of
                        WFCPending timer initTime ->
                            let
                                nextTimer =
                                    timer
                                        |> Quantity.minus delta
                                        |> Quantity.max Quantity.zero

                                builtEnough =
                                    List.length (getBuildHistory model.world.tilemap) >= 5

                                waitedEnough =
                                    (Time.posixToMillis time - Time.posixToMillis initTime) > 2000
                            in
                            if Quantity.lessThanOrEqualToZero nextTimer && (builtEnough || waitedEnough) then
                                startWFC model

                            else
                                ( { model | wfc = WFCPending nextTimer initTime }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        WFCChunkProcessed ( nextTilemap, updatedDrivenWfc, tileActions ) ->
            case model.wfc of
                WFCActive _ ->
                    case updatedDrivenWfc of
                        WFCActive wfc ->
                            ( { model
                                | renderCache = setTilemapDebugCache wfc model.renderCache
                              }
                            , scheduleWFCChunk wfc model.world
                            )

                        WFCFailed wfcLog nextSeed ->
                            let
                                wfc =
                                    restartWfc
                                        nextSeed
                                        (World.tileInventoryCount model.world)
                                        model.world.tilemap
                            in
                            ( { model | debug = appendWfcLog (">Restart WFC (failure)" :: wfcLog) model.debug }
                            , scheduleWFCChunk wfc model.world
                            )

                        WFCSolved wfcLog collapsedTiles nextSeedState ->
                            let
                                audioCmd =
                                    if List.isEmpty collapsedTiles then
                                        Cmd.none

                                    else
                                        playSound Audio.BuildLot

                                nextWorld =
                                    model.world
                                        |> World.setTilemap nextTilemap
                                        |> World.updateSeed nextSeedState
                            in
                            ( { model
                                | world = nextWorld
                                , wfc = updatedDrivenWfc
                                , renderCache = setTilemapCache nextTilemap Nothing model.renderCache
                                , debug = appendWfcLog wfcLog model.debug
                              }
                            , Cmd.batch (audioCmd :: tileActionsToCmds tileActions)
                            )

                        WFCPending _ _ ->
                            ( model, Cmd.none )

                _ ->
                    -- WFC may already have been solved or failed, and the processed chunk is irrelevant
                    ( model, Cmd.none )

        TilemapChangeProcessed lotPlacements ->
            let
                nextWorld =
                    List.foldl
                        (\lotPlacement worldWithLots ->
                            { worldWithLots | tilemap = tilemapWithLotPlacement lotPlacement worldWithLots.tilemap }
                        )
                        model.world
                        lotPlacements

                nextSavegame =
                    Savegame.encode nextWorld
            in
            ( { model
                | world = nextWorld
                , savegame = Just nextSavegame
                , renderCache =
                    if List.isEmpty lotPlacements then
                        model.renderCache

                    else
                        setTilemapCache nextWorld.tilemap Nothing model.renderCache
              }
            , Savegame.updateSavegameUrl nextSavegame
            )

        _ ->
            ( model, Cmd.none )


onPrimaryInput : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
onPrimaryInput cell model =
    if
        Maybe.isNothing (roadTileFromCell cell model.world.tilemap)
            && cellSupportsRoadPlacement cell model.world.tilemap
    then
        addTile cell model

    else
        ( model, Cmd.none )


onSecondaryInput : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
onSecondaryInput cell model =
    if
        fixedTileByCell model.world.tilemap cell
            |> Maybe.andThen extractRoadTile
            |> Maybe.unwrap False isBuilt
    then
        removeTile cell model

    else
        ( model, Cmd.none )



--
-- Tile modification
--


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        { world } =
            model

        tilemapWithMaybeConnectedLargeTileRemoved =
            Tilemap.Core.removeLargeTileIfExists cell world.tilemap

        bitmask =
            cellBitmask cell tilemapWithMaybeConnectedLargeTileRemoved
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            let
                ( withWfc, addTileActions ) =
                    addTileById
                        world.seedState
                        (World.tileInventoryCount world)
                        cell
                        tileId
                        tilemapWithMaybeConnectedLargeTileRemoved
            in
            ( resetDrivenWFC withWfc model
            , Cmd.batch (tileActionsToCmds addTileActions)
            )

        Nothing ->
            ( model, Cmd.none )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    let
        { world } =
            model

        ( updatedWfcModel, actions ) =
            onRemoveTile world.seedState (World.tileInventoryCount world) cell world.tilemap
    in
    ( resetDrivenWFC (WFC.toTilemap updatedWfcModel) model
    , Cmd.batch (playSound Audio.DestroyRoad :: tileActionsToCmds actions)
    )



--
-- WFC
--


startWFC : Liikennematto -> ( Liikennematto, Cmd Message )
startWFC model =
    let
        { world } =
            model

        initialWfc =
            restartWfc world.seedState
                (World.tileInventoryCount world)
                world.tilemap
    in
    ( { model
        | wfc = WFCActive initialWfc
        , debug = appendWfcLog [ ">Restart WFC (timer)" ] model.debug
      }
    , scheduleWFCChunk initialWfc world
    )


scheduleWFCChunk : WFC.Model -> World -> Cmd Message
scheduleWFCChunk wfcModel world =
    -- Small delay to allow for interrupts
    Process.sleep 1
        |> Task.map (\_ -> runWfc world.tilemap wfcModel)
        |> Task.perform WFCChunkProcessed



--
-- External events
--


tilemapWithLotPlacement : LotPlacement -> Tilemap -> Tilemap
tilemapWithLotPlacement { lot, tile, drivewayCell } tilemap =
    let
        constraints =
            getTilemapConfig tilemap

        topLeftCell =
            Tile.largeTileTopLeftCell constraints drivewayCell tile.anchorIndex tile
                -- This fallback is unnecessary - the cell should exist
                |> Maybe.withDefault drivewayCell
    in
    tilemap
        |> mapCell topLeftCell
            (Tile.withName lot.name
                >> Tile.withAnimation (Just Tile.defaultTileAnimation)
            )
        |> addAnimationTimer topLeftCell (Duration.milliseconds 500)



--
-- Helpers
--


resetDrivenWFC : Tilemap -> Liikennematto -> Liikennematto
resetDrivenWFC tilemap model =
    { model
        | world = World.setTilemap tilemap model.world
        , wfc =
            case model.wfc of
                WFCPending _ initTime ->
                    initDrivenWfc initTime

                _ ->
                    initDrivenWfc model.time
        , renderCache = setTilemapCache tilemap (Just tilemap) model.renderCache
    }


tileActionsToCmds : List Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                PlayAudio sound ->
                    playSound sound
        )
