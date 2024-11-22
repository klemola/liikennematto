module Tilemap.Update exposing (addTileById, update)

import Audio exposing (playSound)
import Data.TileSet
    exposing
        ( defaultTiles
        , tileById
        , tileIdByBitmask
        )
import Duration exposing (Duration)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Debug exposing (DevAction(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        , drivenWfcInitialState
        )
import Model.RenderCache as RenderCache exposing (refreshTilemapCache)
import Model.World as World
import Quantity
import Random
import Tilemap.Buffer exposing (updateBufferCells)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , cellBitmask
        , cellSupportsRoadPlacement
        , extractRoadTile
        , fixedTileByCell
        , getTilemapConfig
        , resetSuperposition
        , setSuperpositionOptions
        , tileByCell
        , tileNeighborIn
        , updateTilemap
        )
import Tilemap.DrivenWFC
    exposing
        ( DrivenWFC(..)
        , restartWFC
        , runWFC
        )
import Tilemap.Tile as Tile exposing (Action(..), Tile, TileKind(..), isBuilt)
import Tilemap.TileConfig as TileConfig exposing (TileId)
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
    let
        isRoadTile =
            case extractRoadTile cell model.world.tilemap of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    if not isRoadTile && cellSupportsRoadPlacement cell model.world.tilemap then
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
                    addTileById cell tileId tilemapWithClearedCell world.seed

                withBuffer =
                    updateBufferCells cell withWfc
            in
            ( applyDrivenWfc withBuffer drivenWfcInitialState model
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
    in
    ( WFC.toTilemap updatedWfcModel
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
    ( applyDrivenWfc withWFC drivenWfcInitialState model
    , Cmd.batch (playSound Audio.DestroyRoad :: tileActionsToCmds actions)
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

                -- Does not change Superposition or Uninitialized
                _ ->
                    wfcModel

        Nothing ->
            wfcModel


runWFCIfNecessary : Liikennematto -> Duration -> ( Liikennematto, Cmd Message )
runWFCIfNecessary model delta =
    let
        { world } =
            model
    in
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
                runWFCWithModel model (restartWFC world.seed world.tilemap) False

            else
                ( { model | wfc = WFCPending nextTimer }, Cmd.none )

        WFCActive wfcModel ->
            case WFC.currentState wfcModel of
                WFC.Failed _ ->
                    runWFCWithModel model (restartWFC world.seed world.tilemap) False

                WFC.Done ->
                    runWFCWithModel model wfcModel True

                -- Solving, Recovering
                _ ->
                    let
                        nextWfc =
                            WFC.stepN WFC.StopAtSolved 1000 wfcModel
                    in
                    runWFCWithModel model nextWfc False


runWFCWithModel : Liikennematto -> WFC.Model -> Bool -> ( Liikennematto, Cmd Message )
runWFCWithModel model wfcModel isSolved =
    let
        ( nextTilemap, nextDrivenWfc, tileActions ) =
            runWFC model.world.tilemap wfcModel isSolved
    in
    ( applyDrivenWfc nextTilemap nextDrivenWfc model
    , Cmd.batch (tileActionsToCmds tileActions)
    )



--
-- Helpers
--


applyDrivenWfc : Tilemap -> DrivenWFC -> Liikennematto -> Liikennematto
applyDrivenWfc tilemap drivenWFC model =
    let
        nextWorld =
            World.setTilemap tilemap model.world

        renderCacheWFC =
            case drivenWFC of
                WFCActive wfc ->
                    Just wfc

                _ ->
                    Nothing
    in
    { model
        | world = nextWorld
        , wfc = drivenWFC
        , renderCache = RenderCache.setTilemapCache nextWorld.tilemap renderCacheWFC model.renderCache
    }


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
