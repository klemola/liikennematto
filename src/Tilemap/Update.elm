module Tilemap.Update exposing (update)

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


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        { world, renderCache } =
            model

        bitmask =
            cellBitmask cell world.tilemap
    in
    case tileIdByBitmask bitmask of
        Just tileId ->
            let
                _ =
                    Debug.log "add tile" ( bitmask, tileId, Cell.toString cell )

                ( tilemapWithTile, addTileActions ) =
                    Tilemap.Core.addTile cell tileId world.tilemap

                wfcModel =
                    WFC.fromTilemap tilemapWithTile model.world.seed
                        |> WFC.propagateConstraints cell

                ( withWFC, wfcTileActions ) =
                    collapseNewTileNeighbors cell wfcModel

                nextWorld =
                    { world | tilemap = withWFC }
            in
            ( { model
                | world = nextWorld
                , renderCache = setTilemapCache nextWorld.tilemap renderCache
              }
            , Cmd.batch (tileActionsToCmds (addTileActions ++ wfcTileActions))
            )

        Nothing ->
            ( model, Cmd.none )


collapseNewTileNeighbors : Cell -> WFC.Model -> ( Tilemap, List Action )
collapseNewTileNeighbors cell wfcModel =
    let
        wfcModelAfterCollapse =
            collapseNewTileNeighborsHelper
                cell
                OrthogonalDirection.all
                wfcModel

        ( nextWfcModel, wfcActions ) =
            WFC.flushPendingActions wfcModelAfterCollapse
    in
    ( WFC.toTilemap nextWfcModel
    , wfcActions
    )


collapseNewTileNeighborsHelper : Cell -> List OrthogonalDirection -> WFC.Model -> WFC.Model
collapseNewTileNeighborsHelper origin remainingDirs wfcModel =
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
            collapseNewTileNeighborsHelper origin otherDirs nextWFCModel


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


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    let
        { world, renderCache } =
            model

        ( nextTilemap, tileActions ) =
            Tilemap.Core.removeTile cell world.tilemap

        nextWorld =
            { world | tilemap = nextTilemap }
    in
    ( { model
        | world = nextWorld
        , renderCache = setTilemapCache nextTilemap renderCache
      }
    , Cmd.batch (tileActionsToCmds tileActions)
    )


tileActionsToCmds : List Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                PlayAudio sound ->
                    playSound sound
        )
