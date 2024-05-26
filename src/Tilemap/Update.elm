module Tilemap.Update exposing (modifyTile, update)

import Audio exposing (playSound)
import Data.TileSet exposing (tileIdByBitmask)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        )
import Model.RenderCache exposing (refreshTilemapCache, setTilemapCache)
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
        , tileNeighborIn
        , updateTilemap
        )
import Tilemap.Tile exposing (Action(..), Tile, TileKind(..), isBuilt)
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

                ( nextWorld, maybeTilemapChange ) =
                    { world | tilemap = tilemapUpdateResult.tilemap }
                        |> World.resolveTilemapUpdate delta tilemapUpdateResult

                tilemapChangedEffects =
                    case maybeTilemapChange of
                        Just tilemapChange ->
                            Message.asCmd (TilemapChanged tilemapChange)

                        Nothing ->
                            Cmd.none

                ( nextRenderCache, dynamicTiles ) =
                    refreshTilemapCache tilemapUpdateResult renderCache
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
                , dynamicTiles = dynamicTiles
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds tilemapUpdateResult.actions)
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


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        bitmask =
            cellBitmask cell model.world.tilemap
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            modifyTileAndUpdate cell model (Tilemap.Core.addTile tileId)

        Nothing ->
            ( model, Cmd.none )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    modifyTileAndUpdate cell model Tilemap.Core.removeTile


modifyTileAndUpdate : Cell -> Liikennematto -> (Cell -> Tilemap -> ( Tilemap, List Action )) -> ( Liikennematto, Cmd Message )
modifyTileAndUpdate cell model tilemapChangeFn =
    let
        { world, renderCache } =
            model

        ( withWFC, actions ) =
            modifyTile cell world.tilemap world.seed tilemapChangeFn

        nextWorld =
            { world | tilemap = withWFC }
    in
    ( { model
        | world = nextWorld
        , renderCache = setTilemapCache nextWorld.tilemap renderCache
      }
    , Cmd.batch (tileActionsToCmds actions)
    )


modifyTile : Cell -> Tilemap -> Random.Seed -> (Cell -> Tilemap -> ( Tilemap, List Action )) -> ( Tilemap, List Action )
modifyTile cell tilemap seed tilemapChangeFn =
    let
        ( tilemapWithTile, tilemapChangeActions ) =
            tilemapChangeFn cell tilemap

        wfcModel =
            WFC.fromTilemap tilemapWithTile seed
                |> WFC.propagateConstraints cell

        ( withWFC, wfcTileActions ) =
            collapseTileNeighbors cell wfcModel
    in
    ( withWFC
    , tilemapChangeActions ++ wfcTileActions
    )


collapseTileNeighbors : Cell -> WFC.Model -> ( Tilemap, List Action )
collapseTileNeighbors cell wfcModel =
    let
        wfcModelAfterCollapse =
            collapseTileNeighborsHelper
                cell
                OrthogonalDirection.all
                wfcModel

        ( nextWfcModel, wfcActions ) =
            WFC.flushPendingActions wfcModelAfterCollapse
    in
    ( WFC.toTilemap nextWfcModel
    , wfcActions
    )


collapseTileNeighborsHelper : Cell -> List OrthogonalDirection -> WFC.Model -> WFC.Model
collapseTileNeighborsHelper origin remainingDirs wfcModel =
    case remainingDirs of
        [] ->
            wfcModel

        dir :: otherDirs ->
            let
                maybeTile =
                    tileNeighborIn dir origin (WFC.toTilemap wfcModel)

                nextWFCModel =
                    processTileNeighbor maybeTile wfcModel
            in
            collapseTileNeighborsHelper origin otherDirs nextWFCModel


processTileNeighbor : Maybe ( Cell, Tile ) -> WFC.Model -> WFC.Model
processTileNeighbor maybeTile wfcModel =
    case maybeTile of
        Just ( cell, tile ) ->
            case tile.kind of
                Fixed _ ->
                    wfcModel
                        |> WFC.resetCell cell
                        |> WFC.collapse cell

                Superposition _ ->
                    wfcModel

        Nothing ->
            wfcModel



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
        )
