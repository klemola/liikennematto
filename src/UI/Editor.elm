port module UI.Editor exposing
    ( carSpawnControl
    , overlay
    , toolbar
    , update
    , zoomControl
    )

import Browser.Dom
import CustomEvent
import Data.Defaults as Defaults
import Duration exposing (Duration)
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Cell as Cell exposing (Cell)
import Model.Editor as Editor exposing (Editor, Tool(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.RenderCache as RenderCache exposing (RenderCache, refreshTilemapCache)
import Model.Tile as Tile
import Model.Tilemap as Tilemap exposing (TilemapUpdateResult)
import Model.World as World exposing (World)
import Quantity
import Random
import Render.Conversion
import Simulation.Traffic as Traffic
import Task
import UI.Core
    exposing
        ( borderRadius
        , borderSize
        , colors
        , containerId
        , controlButton
        , icon
        , scrollbarAwareOffsetF
        , uiDimensions
        , whitespace
        )


port playAudio : String -> Cmd msg



-- Update


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        SelectTool tool ->
            let
                nextTool =
                    if model.editor.tool == tool then
                        SmartConstruction

                    else
                        tool

                nextEditor =
                    Editor.activateTool nextTool model.editor
            in
            ( { model | editor = nextEditor }, Cmd.none )

        ChangeZoomLevel nextLevel ->
            let
                nextEditor =
                    Editor.setZoomLevel nextLevel model.editor

                nextRenderCache =
                    RenderCache.setPixelsToMetersRatio nextEditor.zoomLevel model.renderCache
            in
            ( { model
                | editor = nextEditor
                , renderCache = nextRenderCache
              }
            , Browser.Dom.getViewportOf containerId
                |> Task.andThen
                    (\domViewport ->
                        let
                            mapSizeChangeX =
                                nextRenderCache.tilemapWidthPixels - model.renderCache.tilemapWidthPixels

                            mapSizeChangeY =
                                nextRenderCache.tilemapHeightPixels - model.renderCache.tilemapHeightPixels

                            nextScrollX =
                                (mapSizeChangeX / 2) + domViewport.viewport.x

                            nextScrollY =
                                (mapSizeChangeY / 2) + domViewport.viewport.y
                        in
                        Browser.Dom.setViewportOf containerId (max nextScrollX 0) (max nextScrollY 0)
                    )
                |> Task.attempt (\_ -> NoOp)
            )

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
                { world, renderCache } =
                    model

                tilemapUpdateResult =
                    Tilemap.update delta world.tilemap

                nextWorld =
                    { world | tilemap = tilemapUpdateResult.tilemap }

                nextRenderCache =
                    if List.isEmpty tilemapUpdateResult.changedCells then
                        renderCache

                    else
                        refreshTilemapCache tilemapUpdateResult.tilemap renderCache

                ( nextEditor, tilemapChangedEffects ) =
                    resolveTilemapUpdate delta tilemapUpdateResult model
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
                , editor = nextEditor
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


resolveTilemapUpdate : Duration -> TilemapUpdateResult -> Liikennematto -> ( Editor, Cmd Message )
resolveTilemapUpdate delta tilemapUpdateResult model =
    let
        { editor } =
            model
    in
    case editor.pendingTilemapChange of
        Nothing ->
            let
                nextEditor =
                    if List.isEmpty tilemapUpdateResult.changedCells then
                        editor

                    else
                        Editor.createPendingTilemapChange tilemapUpdateResult.changedCells editor
            in
            ( nextEditor
            , Cmd.none
            )

        Just pendingTilemapChange ->
            let
                ( changeTimer, currentChangedCells ) =
                    pendingTilemapChange

                nextTimer =
                    if not (List.isEmpty tilemapUpdateResult.changedCells) then
                        -- The tilemap changed during the delay, reset it (AKA debounce)
                        Editor.minTilemapChangeFrequency

                    else
                        changeTimer
                            |> Quantity.minus delta
                            |> Quantity.max Quantity.zero

                nextChangedCells =
                    Editor.combineChangedCells tilemapUpdateResult.changedCells currentChangedCells
            in
            if Quantity.lessThanOrEqualToZero nextTimer then
                ( { editor | pendingTilemapChange = Nothing }
                , nextChangedCells
                    |> Cell.fromCoordinatesSet (Tilemap.config model.world.tilemap)
                    |> Task.succeed
                    |> Task.perform TilemapChanged
                )

            else
                ( { editor | pendingTilemapChange = Just ( nextTimer, nextChangedCells ) }
                , Cmd.none
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


overlay : RenderCache -> World -> Editor -> Element Message
overlay cache world editor =
    let
        { tool } =
            editor

        tileSizePixels =
            Render.Conversion.toPixelsValue cache.pixelsToMetersRatio Cell.size

        cellSize =
            tileSizePixels
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
                        , cellSize = cellSize
                        , tileSizePixels = tileSizePixels
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
        [ Element.mouseOver [ Border.innerGlow highlight tileSizePixels ]
        , Element.width cellSize
        , Element.height cellSize
        ]
        (Element.column [] rows)


tileOverlay :
    { glowColor : Maybe Color
    , cell : Cell
    , world : World
    , tool : Tool
    , tileSizePixels : Float
    , cellSize : Element.Length
    }
    -> Element Message
tileOverlay { glowColor, cell, world, tool, tileSizePixels, cellSize } =
    let
        glow =
            glowColor
                |> Maybe.map
                    (\color ->
                        [ Border.innerGlow color (tileSizePixels / 10) ]
                    )
                |> Maybe.withDefault []
    in
    Element.el
        [ Element.width cellSize
        , Element.height cellSize
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


toolbar : Editor -> Element Message
toolbar editor =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignLeft
        ]
        [ toolbarButton editor.tool Bulldozer "bulldozer.png"
        , toolbarButton editor.tool Dynamite "dynamite.png"
        ]


toolbarButton : Tool -> Tool -> String -> Element Message
toolbarButton selectedTool tool asset =
    controlButton
        { label = icon asset
        , onPress = SelectTool tool
        , selected = selectedTool == tool
        , disabled = False
        }


carSpawnControl : Editor -> Element Message
carSpawnControl editor =
    let
        disabled =
            editor.carSpawnQueue >= Editor.maxQueuedCars
    in
    controlButton
        { label = Element.text "🚗"
        , onPress = SpawnTestCar
        , selected = False
        , disabled = disabled
        }


zoomControl : Editor -> Element Message
zoomControl editor =
    let
        baseWidth =
            uiDimensions.zoomControlWidth

        baseHeight =
            uiDimensions.zoomTrackHeight

        paddingX =
            whitespace.regular

        paddingY =
            whitespace.regular

        sliderWidth =
            baseWidth - (2 * paddingX)

        sliderHeight =
            baseHeight - (2 * paddingY)

        thumbWidth =
            uiDimensions.zoomTrackWidth + paddingX

        thumbHeight =
            uiDimensions.zoomTrackWidth
    in
    Element.el
        [ Element.paddingXY paddingX paddingY
        , Element.width (Element.px baseWidth)
        , Element.height (Element.px baseHeight)
        , Element.alignLeft
        , Element.moveRight scrollbarAwareOffsetF
        , Element.moveUp scrollbarAwareOffsetF
        , Element.alignBottom
        , Background.color colors.menuBackground
        , Border.rounded borderRadius.light
        ]
        (Input.slider
            [ Element.width (Element.px sliderWidth)
            , Element.height (Element.px sliderHeight)
            , Element.behindContent track
            ]
            { onChange = ChangeZoomLevel
            , label = Input.labelHidden "Zoom"
            , min = 1
            , max = 3
            , step = Just 1
            , value = Editor.zoomLevelToUIValue editor.zoomLevel
            , thumb =
                Input.thumb
                    [ Element.width (Element.px thumbWidth)
                    , Element.height (Element.px thumbHeight)
                    , Border.rounded borderSize.light
                    , Background.color colors.inputBackground
                    ]
            }
        )


track : Element Message
track =
    let
        stepGuide =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px whitespace.regular)
                , Background.color colors.inputBackground
                ]
                Element.none
    in
    Element.el
        [ Element.width (Element.px uiDimensions.zoomTrackWidth)
        , Element.height Element.fill
        , Element.centerX
        , Background.color colors.mainBackground
        , Border.solid
        , Border.width borderSize.light
        , Border.color colors.heavyBorder
        , Border.rounded borderRadius.light
        , Element.inFront
            (Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.spaceEvenly
                ]
                [ stepGuide
                , stepGuide
                , stepGuide
                ]
            )
        ]
        Element.none
