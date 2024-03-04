module Tilemap.Update exposing (update)

import Audio exposing (playSound)
import Message exposing (Message(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        , SimulationState(..)
        )
import Model.RenderCache exposing (refreshTilemapCache)
import Model.World as World
import Simulation.Zoning exposing (removeInvalidLots)
import Task
import Tilemap.Core exposing (updateTilemap)
import Tilemap.Tile exposing (Action(..))


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

        _ ->
            ( model, Cmd.none )


tileActionsToCmds : List Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                PlayAudio sound ->
                    playSound sound
        )
