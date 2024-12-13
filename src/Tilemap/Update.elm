module Tilemap.Update exposing (addTileById, update)

import Audio exposing (playSound)
import Data.TileSet
    exposing
        ( defaultTiles
        , tileIdByBitmask
        )
import Duration exposing (Duration)
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Debug exposing (DevAction(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache exposing (refreshTilemapCache, setTilemapCache, setTilemapDebugCache)
import Model.World as World exposing (World)
import Quantity
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
        , updateTilemap
        )
import Tilemap.DrivenWFC
    exposing
        ( DrivenWFC(..)
        , drivenWfcInitialState
        , resetWFC
        , restartWFC
        , runWFC
        , updateTileNeighbors
        )
import Tilemap.Tile as Tile
    exposing
        ( Action(..)
        , TileKind(..)
        , isBuilt
        )
import Tilemap.TileConfig exposing (TileId)
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
                    addTileById cell tileId world tilemapWithClearedCell

                withBuffer =
                    updateBufferCells cell withWfc
            in
            ( resetDrivenWFC withBuffer model
            , Cmd.batch (tileActionsToCmds (clearCellActions ++ addTileActions))
            )

        Nothing ->
            ( model, Cmd.none )


addTileById : Cell -> TileId -> World -> Tilemap -> ( Tilemap, List Action )
addTileById cell tileId world tilemap =
    let
        ( updatedTilemap, tilemapChangeActions ) =
            Tilemap.Core.addTile tileId cell tilemap

        wfcModel =
            resetWFC world.seed (Just cell) (World.unavailableTileIds world) updatedTilemap

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
            resetWFC world.seed (Just cell) (World.unavailableTileIds world) updatedTilemap

        ( updatedWfcModel, wfcTileActions ) =
            updateTileNeighbors cell wfcModel

        ( withWFC, actions ) =
            ( WFC.toTilemap updatedWfcModel
            , tilemapChangeActions ++ wfcTileActions
            )
    in
    ( resetDrivenWFC withWFC model
    , Cmd.batch (playSound Audio.DestroyRoad :: tileActionsToCmds actions)
    )


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
                runWFCWithModel
                    (restartWFC world.seed
                        (World.unavailableTileIds world)
                        world.tilemap
                    )
                    model

            else
                ( { model | wfc = WFCPending nextTimer }, Cmd.none )

        WFCActive wfcModel ->
            runWFCWithModel wfcModel model


runWFCWithModel : WFC.Model -> Liikennematto -> ( Liikennematto, Cmd Message )
runWFCWithModel wfcModel model =
    let
        { world } =
            model

        ( nextTilemap, nextDrivenWfc, tileActions ) =
            runWFC world.seed world.tilemap wfcModel
    in
    case nextDrivenWfc of
        WFCActive wfc ->
            ( { model
                | wfc = nextDrivenWfc
                , renderCache = setTilemapDebugCache wfc model.renderCache
              }
            , Cmd.none
            )

        WFCSolved ->
            ( { model
                | world = World.setTilemap nextTilemap world
                , wfc = nextDrivenWfc
                , renderCache = setTilemapCache nextTilemap Nothing model.renderCache
              }
            , Cmd.batch (tileActionsToCmds tileActions)
            )

        WFCPending _ ->
            -- This branch is handled at the call site
            ( model, Cmd.none )



--
-- Helpers
--


resetDrivenWFC : Tilemap -> Liikennematto -> Liikennematto
resetDrivenWFC tilemap model =
    { model
        | world = World.setTilemap tilemap model.world
        , wfc = drivenWfcInitialState
        , renderCache = setTilemapCache tilemap (Just tilemap) model.renderCache
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
