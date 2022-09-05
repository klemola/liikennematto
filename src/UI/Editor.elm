port module UI.Editor exposing
    ( carSpawnControl
    , overlay
    , toolbar
    , update
    )

import CustomEvent
import Data.Defaults as Defaults
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Cell as Cell exposing (Cell)
import Model.Editor as Editor exposing (Editor, Tool(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache as RenderCache exposing (refreshTilemapCache)
import Model.Tile as Tile
import Model.Tilemap as Tilemap
import Model.World as World exposing (World)
import Random
import Render
import Simulation.Traffic as Traffic
import Task
import UI.Core
    exposing
        ( ControlButtonSize
        , colors
        , controlButton
        , icon
        , whitespace
        )


port playAudio : String -> Cmd msg



-- Update


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        SelectTool tool ->
            let
                nextEditor =
                    model.editor
                        |> Editor.activateTool
                            (if model.editor.tool == tool then
                                None

                             else
                                tool
                            )
            in
            ( { model | editor = nextEditor }, Cmd.none )

        AddTile cell ->
            let
                { world } =
                    model

                ( nextTilemap, tileActions ) =
                    Tilemap.addTile cell world.tilemap

                nextWorld =
                    { world | tilemap = nextTilemap }
            in
            ( { model
                | world = nextWorld
                , renderCache = refreshTilemapCache nextTilemap model.renderCache
              }
            , Cmd.batch (tileActionsToCmds tileActions)
            )

        RemoveTile cell ->
            let
                { world } =
                    model

                ( nextTilemap, tileActions ) =
                    Tilemap.removeTile cell model.world.tilemap

                nextWorld =
                    { world | tilemap = nextTilemap }
            in
            ( { model
                | world = nextWorld
                , renderCache = refreshTilemapCache nextTilemap model.renderCache
              }
            , Cmd.batch (tileActionsToCmds tileActions)
            )

        UpdateTilemap delta ->
            let
                { world } =
                    model

                tilemapUpdateResult =
                    Tilemap.update delta world.tilemap

                nextWorld =
                    { world | tilemap = tilemapUpdateResult.tilemap }

                ( nextRenderCache, tilemapChangedEffects ) =
                    if List.isEmpty tilemapUpdateResult.changedCells then
                        ( model.renderCache, Cmd.none )

                    else
                        ( refreshTilemapCache tilemapUpdateResult.tilemap model.renderCache
                        , tilemapUpdateResult.changedCells
                            |> Task.succeed
                            |> Task.perform TilemapChanged
                        )
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds tilemapUpdateResult.actions)
            )

        ResetWorld ->
            let
                nextWorld =
                    World.empty
                        { horizontalCellsAmount = Defaults.horizontalCellsAmount
                        , verticalCellsAmount = Defaults.verticalCellsAmount
                        }

                nextEditor =
                    Editor.activateTool SmartConstruction model.editor
            in
            ( { model
                | editor = nextEditor
                , world = nextWorld
                , renderCache = RenderCache.new nextWorld
              }
            , Cmd.none
            )

        CheckQueues ->
            let
                { carSpawnQueue } =
                    model.editor

                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn carSpawnQueue model.seed model.world

                nextEditor =
                    Editor.setCarSpawnQueue nextCarSpawnQueue model.editor
            in
            ( { model
                | world = nextWorld
                , seed = nextSeed
                , editor = nextEditor
              }
            , Cmd.none
            )

        SpawnTestCar ->
            let
                nextEditor =
                    Editor.setCarSpawnQueue (model.editor.carSpawnQueue + 1) model.editor
            in
            ( { model | editor = nextEditor }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


tileActionsToCmds : List Tile.Action -> List (Cmd Message)
tileActionsToCmds =
    List.map
        (\action ->
            case action of
                Tile.PlayAudio sound ->
                    playAudio sound
        )


dequeueCarSpawn : Int -> Random.Seed -> World -> ( World, Int, Random.Seed )
dequeueCarSpawn queue seed world =
    let
        canSpawnCar =
            queue > 0
    in
    if canSpawnCar then
        let
            ( nextWorld, nextSeed, newCarId ) =
                Traffic.spawnCar seed world
        in
        case newCarId of
            Just _ ->
                ( nextWorld, queue - 1, nextSeed )

            Nothing ->
                ( nextWorld, queue, nextSeed )

    else
        ( world, queue, seed )



-- Views


overlay : World -> Editor -> Element Message
overlay world editor =
    let
        { tool } =
            editor

        size =
            Render.tileSizePixels
                |> floor
                |> Element.px

        tilemapConfig =
            Tilemap.config world.tilemap

        cellElement x y =
            case
                Cell.fromCoordinates tilemapConfig ( x, y )
            of
                Just cell ->
                    tileOverlay
                        { glowColor =
                            tileHighlight
                                { world = world
                                , selectedTool = tool
                                , cell = cell
                                }
                        , cell = cell
                        , world = world
                        , tool = tool
                        }

                Nothing ->
                    Element.none

        rows =
            List.map
                (\y ->
                    Element.row []
                        (List.map
                            (\x -> cellElement x y)
                            (List.range 1 tilemapConfig.verticalCellsAmount)
                        )
                )
                (List.range 1 tilemapConfig.horizontalCellsAmount)

        highlight =
            case tool of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    Element.el
        [ Element.mouseOver [ Border.innerGlow highlight Render.tileSizePixels ]
        , Element.width size
        , Element.height size
        ]
        (Element.column [] rows)


tileOverlay :
    { glowColor : Maybe Color
    , cell : Cell
    , world : World
    , tool : Tool
    }
    -> Element Message
tileOverlay { glowColor, cell, world, tool } =
    let
        overlaySize =
            Render.tileSizePixels
                |> floor
                |> Element.px

        glow =
            glowColor
                |> Maybe.map
                    (\color ->
                        [ Border.innerGlow color (Render.tileSizePixels / 10) ]
                    )
                |> Maybe.withDefault []
    in
    Element.el
        [ Element.width overlaySize
        , Element.height overlaySize
        , Element.mouseOver glow
        , Events.onClick (choosePrimaryAction cell tool world)
        , Element.htmlAttribute (CustomEvent.onRightClick <| chooseSecondaryAction cell tool world)
        ]
        Element.none


choosePrimaryAction : Cell -> Tool -> World -> Message
choosePrimaryAction cell tool world =
    case ( tool, Tilemap.tileAt world.tilemap cell ) of
        ( SmartConstruction, _ ) ->
            let
                alreadyExists =
                    Tilemap.exists cell world.tilemap
            in
            if not alreadyExists && Tilemap.canBuildRoadAt cell world.tilemap then
                AddTile cell

            else
                NoOp

        ( Bulldozer, Just _ ) ->
            let
                tile =
                    Tilemap.tileAt world.tilemap cell
            in
            if Maybe.unwrap False Tile.isBuilt tile then
                RemoveTile cell

            else
                NoOp

        ( Dynamite, _ ) ->
            ResetWorld

        _ ->
            NoOp


chooseSecondaryAction : Cell -> Tool -> World -> Message
chooseSecondaryAction cell tool world =
    case tool of
        SmartConstruction ->
            let
                tile =
                    Tilemap.tileAt world.tilemap cell
            in
            if Maybe.unwrap False Tile.isBuilt tile then
                RemoveTile cell

            else
                NoOp

        _ ->
            NoOp


tileHighlight :
    { world : World
    , selectedTool : Tool
    , cell : Cell
    }
    -> Maybe Color
tileHighlight { world, selectedTool, cell } =
    case selectedTool of
        Bulldozer ->
            if Tilemap.exists cell world.tilemap then
                Just colors.danger

            else
                Nothing

        SmartConstruction ->
            let
                canBuildHere =
                    Tilemap.canBuildRoadAt cell world.tilemap

                mightDestroyLot =
                    World.hasLot cell world
            in
            if canBuildHere && mightDestroyLot then
                Just colors.danger

            else if canBuildHere then
                Just colors.target

            else
                Just colors.notAllowed

        _ ->
            Nothing


toolbar : Editor -> ControlButtonSize -> Element Message
toolbar editor controlButtonSize =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignLeft
        ]
        [ toolbarButton editor.tool SmartConstruction controlButtonSize
        , toolbarButton editor.tool Bulldozer controlButtonSize
        , toolbarButton editor.tool Dynamite controlButtonSize
        ]


toolbarButton : Tool -> Tool -> ControlButtonSize -> Element Message
toolbarButton selectedTool tool controlButtonSize =
    let
        asset =
            case tool of
                SmartConstruction ->
                    "smart_construction.png"

                Bulldozer ->
                    "bulldozer.png"

                Dynamite ->
                    "dynamite.png"

                None ->
                    "__none__"
    in
    controlButton
        { label = icon asset
        , onPress = SelectTool tool
        , selected = selectedTool == tool
        , disabled = False
        , size = controlButtonSize
        }


carSpawnControl : Editor -> ControlButtonSize -> Element Message
carSpawnControl editor controlButtonSize =
    let
        disabled =
            editor.carSpawnQueue >= Editor.maxQueuedCars
    in
    controlButton
        { label = Element.text "🚗"
        , onPress = SpawnTestCar
        , selected = False
        , disabled = disabled
        , size = controlButtonSize
        }
