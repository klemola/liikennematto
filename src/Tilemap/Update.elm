module Tilemap.Update exposing (update)

import Audio exposing (playSound)
import Data.TileSet
    exposing
        ( defaultTiles
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
import Task
import Tilemap.Buffer exposing (updateBufferCells)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , addAnchor
        , addAnimationTimer
        , cellBitmask
        , cellSupportsRoadPlacement
        , extractRoadTile
        , fixedTileByCell
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
                    case model.wfc of
                        WFCPending timer ->
                            let
                                nextTimer =
                                    timer
                                        |> Quantity.minus delta
                                        |> Quantity.max Quantity.zero
                            in
                            if Quantity.lessThanOrEqualToZero nextTimer then
                                let
                                    { world } =
                                        model

                                    initialWfc =
                                        restartWfc world.seed
                                            (World.tileInventoryCount world)
                                            world.tilemap
                                in
                                ( { model
                                    | wfc = WFCActive initialWfc
                                    , debug = appendWfcLog [ ">Restart WFC (timer)" ] model.debug
                                  }
                                , scheduleWFCChunk initialWfc world
                                )

                            else
                                ( { model | wfc = WFCPending nextTimer }, Cmd.none )

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
                                        nextTilemap
                            in
                            ( { model | debug = appendWfcLog (">Restart WFC (failure)" :: wfcLog) model.debug }
                            , scheduleWFCChunk wfc model.world
                            )

                        WFCSolved wfcLog collapsedTiles ->
                            let
                                audioCmd =
                                    if List.isEmpty collapsedTiles then
                                        Cmd.none

                                    else
                                        playSound Audio.BuildLot
                            in
                            ( { model
                                | world = World.setTilemap nextTilemap model.world
                                , wfc = updatedDrivenWfc
                                , renderCache = setTilemapCache nextTilemap Nothing model.renderCache
                                , debug = appendWfcLog wfcLog model.debug
                              }
                            , Cmd.batch (audioCmd :: tileActionsToCmds tileActions)
                            )

                        WFCPending _ ->
                            ( model, Cmd.none )

                _ ->
                    -- WFC may already have been solved or failed, and the processed chunk is irrelevant
                    ( model, Cmd.none )

        LotsPlaced lotPlacements ->
            let
                nextWorld =
                    List.foldl
                        (\lotPlacement worldWithLots ->
                            { worldWithLots | tilemap = tilemapWithLotPlacement lotPlacement worldWithLots.tilemap }
                        )
                        model.world
                        lotPlacements
            in
            ( { model
                | world = nextWorld
                , renderCache = setTilemapCache nextWorld.tilemap Nothing model.renderCache
              }
            , Cmd.none
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

        ( tilemapWithClearedCell, clearCellActions ) =
            Tilemap.Core.removeTile cell world.tilemap

        bitmask =
            cellBitmask cell tilemapWithClearedCell
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            let
                ( withWfc, addTileActions ) =
                    addTileById world.seed (World.tileInventoryCount world) cell tileId tilemapWithClearedCell

                withBuffer =
                    updateBufferCells cell withWfc
            in
            ( resetDrivenWFC withBuffer model
            , Cmd.batch (tileActionsToCmds (clearCellActions ++ addTileActions))
            )

        Nothing ->
            ( model, Cmd.none )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    let
        { world } =
            model

        ( updatedWfcModel, actions ) =
            onRemoveTile world.seed (World.tileInventoryCount world) cell world.tilemap
    in
    ( resetDrivenWFC (WFC.toTilemap updatedWfcModel) model
    , Cmd.batch (playSound Audio.DestroyRoad :: tileActionsToCmds actions)
    )



--
-- WFC
--


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
tilemapWithLotPlacement { lot, tile, drivewayCell, anchorCell } tilemap =
    let
        constraints =
            getTilemapConfig tilemap

        topLeftCell =
            Tile.largeTileTopLeftCell constraints drivewayCell tile.anchorIndex tile
                -- This fallback is unnecessary - the cell should exist
                |> Maybe.withDefault drivewayCell
    in
    tilemap
        |> addAnchor anchorCell
            lot.id
            lot.entryDirection
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
        , wfc = initDrivenWfc
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
