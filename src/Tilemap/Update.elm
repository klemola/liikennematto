module Tilemap.Update exposing (update)

import Audio exposing (playSound)
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        )
import Model.RenderCache exposing (refreshTilemapCache, setTilemapCache)
import Model.World as World
import Simulation.Zoning exposing (removeInvalidLots)
import Task
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core exposing (canBuildRoadAt, cellHasFixedTile, fixedTileByCell, getTilemapConfig, updateTilemap)
import Tilemap.Tile exposing (Action(..), isBuilt)
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

                ( nextWorld, changedCells ) =
                    { world | tilemap = tilemapUpdateResult.tilemap }
                        |> removeInvalidLots tilemapUpdateResult.transitionedCells
                        |> World.resolveTilemapUpdate delta tilemapUpdateResult

                tilemapChangedEffects =
                    if List.isEmpty changedCells then
                        Cmd.none

                    else
                        Task.succeed changedCells
                            |> Task.perform TilemapChanged

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

        ( nextTilemap, tileActions ) =
            Tilemap.Core.addTile cell world.tilemap

        nextWorld =
            { world | tilemap = nextTilemap }
    in
    ( { model
        | world = nextWorld
        , renderCache = setTilemapCache nextTilemap renderCache
      }
    , Cmd.batch (tileActionsToCmds tileActions)
    )


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
