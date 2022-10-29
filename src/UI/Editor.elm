port module UI.Editor exposing
    ( overlay
    , update
    , zoomControl
    )

import Browser.Dom
import Data.Defaults as Defaults
import Duration exposing (Duration)
import Element exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Cell as Cell exposing (Cell)
import Model.Editor as Editor exposing (Editor)
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
        , overlayId
        , scrollbarAwareOffsetF
        , uiDimensions
        , whitespace
        )
import UI.TimerIndicator


port playAudio : String -> Cmd msg


type OverlayIntent
    = AddTile
    | RemoveTile
    | NoIntent


longTapThreshold : Duration
longTapThreshold =
    Duration.milliseconds 1500


longTapIndicatorShowDelay : Duration
longTapIndicatorShowDelay =
    Duration.milliseconds 300



-- Update


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    let
        { editor, world, renderCache } =
            model

        tilemapConfig =
            Tilemap.config world.tilemap
    in
    case msg of
        ChangeZoomLevel nextLevel ->
            let
                nextEditor =
                    Editor.setZoomLevel nextLevel editor

                nextRenderCache =
                    RenderCache.setPixelsToMetersRatio nextEditor.zoomLevel renderCache
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
                                nextRenderCache.tilemapWidthPixels - renderCache.tilemapWidthPixels

                            mapSizeChangeY =
                                nextRenderCache.tilemapHeightPixels - renderCache.tilemapHeightPixels

                            nextScrollX =
                                (mapSizeChangeX / 2) + domViewport.viewport.x

                            nextScrollY =
                                (mapSizeChangeY / 2) + domViewport.viewport.y
                        in
                        Browser.Dom.setViewportOf containerId (max nextScrollX 0) (max nextScrollY 0)
                    )
                |> Task.attempt (\_ -> NoOp)
            )

        UpdateTilemap delta ->
            let
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

        CheckQueues ->
            let
                { carSpawnQueue } =
                    editor

                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn carSpawnQueue model.seed world

                nextEditor =
                    Editor.setCarSpawnQueue nextCarSpawnQueue editor
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
                    Editor.setCarSpawnQueue (editor.carSpawnQueue + 1) editor
            in
            ( { model | editor = nextEditor }
            , Cmd.none
            )

        ResetWorld ->
            let
                nextWorld =
                    World.empty
                        { horizontalCellsAmount = Defaults.horizontalCellsAmount
                        , verticalCellsAmount = Defaults.verticalCellsAmount
                        }

                baseEditor =
                    Editor.new
            in
            ( { model
                | editor =
                    { baseEditor
                        | lastEventDevice = model.editor.lastEventDevice
                        , zoomLevel = model.editor.zoomLevel
                    }
                , world = nextWorld
                , renderCache = RenderCache.new nextWorld
              }
            , Cmd.none
            )

        AnimationFrameReceived delta ->
            ( { model | editor = Editor.advanceLongPressTimer delta editor }
            , Cmd.none
            )

        OverlayPointerMove event ->
            case
                pointerEventToCell
                    renderCache
                    tilemapConfig
                    event
            of
                Just activeCell ->
                    ( { model | editor = Editor.activateCell activeCell editor }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        OverlayPointerLeave event ->
            ( { model
                | editor =
                    editor
                        |> Editor.setLastEventDevice event.pointerType
                        |> Editor.deactivateCell
              }
            , Cmd.none
            )

        OverlayPointerDown event ->
            case pointerEventToCell renderCache tilemapConfig event of
                Just eventCell ->
                    let
                        cellHasTile =
                            Tilemap.exists eventCell world.tilemap
                    in
                    ( { model | editor = Editor.selectCell event eventCell cellHasTile editor }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | editor = Editor.deactivateCell editor }
                    , Cmd.none
                    )

        OverlayPointerUp event ->
            let
                modelWithEditorUpdate =
                    { model
                        | editor =
                            editor
                                |> Editor.clearPointerDownEvent
                                |> Editor.resetLongPressTimer
                    }
            in
            case pointerEventToCell renderCache tilemapConfig event of
                Just pointerUpCell ->
                    let
                        pointerDownCell =
                            editor.pointerDownEvent |> Maybe.andThen (pointerEventToCell renderCache tilemapConfig)
                    in
                    applyOverlayIntent
                        (resolvePointerUp
                            pointerDownCell
                            pointerUpCell
                            event
                            world
                            editor
                        )
                        pointerUpCell
                        modelWithEditorUpdate

                Nothing ->
                    ( modelWithEditorUpdate, Cmd.none )

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


pointerEventToCell : RenderCache -> Tilemap.TilemapConfig -> Pointer.Event -> Maybe Cell
pointerEventToCell cache constraints event =
    let
        ( overlayX, overlayY ) =
            event.pointer.offsetPos

        cellMetersValue =
            Quantity.unwrap Cell.size

        overlayXMetersValue =
            overlayX
                |> Render.Conversion.toMetersValue cache.pixelsToMetersRatio
                |> Quantity.unwrap

        overlayYMetersValue =
            overlayY
                |> Render.Conversion.toMetersValue cache.pixelsToMetersRatio
                |> Quantity.unwrap

        coordinates =
            ( 1 + floor (overlayXMetersValue / cellMetersValue)
            , 1 + floor (overlayYMetersValue / cellMetersValue)
            )
    in
    Cell.fromCoordinates constraints coordinates


resolvePointerUp : Maybe Cell -> Cell -> Pointer.Event -> World -> Editor -> OverlayIntent
resolvePointerUp pointerDownCell pointerUpCell pointerUpEvent world editor =
    let
        validRelease =
            pointerDownCell
                |> Maybe.map (Cell.identical pointerUpCell)
                |> Maybe.withDefault False

        isRightClick =
            pointerUpEvent.pointer.button == Mouse.SecondButton

        isLongTap =
            case editor.longPressTimer of
                Just elapsed ->
                    validRelease && (elapsed |> Quantity.greaterThanOrEqualTo longTapThreshold)

                Nothing ->
                    False
    in
    if isLongTap || isRightClick then
        chooseSecondaryIntent pointerUpCell world

    else if validRelease then
        choosePrimaryIntent pointerUpCell world

    else
        NoIntent


choosePrimaryIntent : Cell -> World -> OverlayIntent
choosePrimaryIntent cell world =
    let
        alreadyExists =
            Tilemap.exists cell world.tilemap
    in
    if not alreadyExists && Tilemap.canBuildRoadAt cell world.tilemap then
        AddTile

    else
        NoIntent


chooseSecondaryIntent : Cell -> World -> OverlayIntent
chooseSecondaryIntent cell world =
    let
        tile =
            Tilemap.tileAt world.tilemap cell
    in
    if Maybe.unwrap False Tile.isBuilt tile then
        RemoveTile

    else
        NoIntent


applyOverlayIntent : OverlayIntent -> Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
applyOverlayIntent intent activeCell model =
    case intent of
        AddTile ->
            addTile activeCell model

        RemoveTile ->
            removeTile activeCell model

        NoIntent ->
            ( model, Cmd.none )


addTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
addTile cell model =
    let
        { world, renderCache } =
            model

        ( nextTilemap, tileActions ) =
            Tilemap.addTile cell world.tilemap

        nextWorld =
            { world | tilemap = nextTilemap }
    in
    ( { model
        | world = nextWorld
        , editor = Editor.activateCell cell model.editor
        , renderCache = refreshTilemapCache nextTilemap renderCache
      }
    , Cmd.batch (tileActionsToCmds tileActions)
    )


removeTile : Cell -> Liikennematto -> ( Liikennematto, Cmd Message )
removeTile cell model =
    let
        { world, renderCache } =
            model

        ( nextTilemap, tileActions ) =
            Tilemap.removeTile cell world.tilemap

        nextWorld =
            { world | tilemap = nextTilemap }
    in
    ( { model
        | world = nextWorld
        , editor = Editor.activateCell cell model.editor
        , renderCache = refreshTilemapCache nextTilemap renderCache
      }
    , Cmd.batch (tileActionsToCmds tileActions)
    )



-- Views


overlay : RenderCache -> World -> Editor -> Element Message
overlay cache world editor =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Html.Attributes.id overlayId)
        , Element.inFront
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.htmlAttribute (Pointer.onMove OverlayPointerMove)
                , Element.htmlAttribute (Pointer.onLeave OverlayPointerLeave)
                , Element.htmlAttribute (Pointer.onDown OverlayPointerDown)
                , Element.htmlAttribute (Pointer.onUp OverlayPointerUp)
                , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
                ]
                Element.none
            )
        ]
        (case editor.activeCell of
            Just cell ->
                if editor.lastEventDevice == Pointer.MouseType then
                    cellHighlight cache world cell

                else
                    case editor.longPressTimer of
                        Just elapsed ->
                            UI.TimerIndicator.view
                                longTapThreshold
                                longTapIndicatorShowDelay
                                cache.pixelsToMetersRatio
                                (Cell.coordinates cell)
                                elapsed

                        Nothing ->
                            Element.none

            Nothing ->
                Element.none
        )


cellHighlight : RenderCache -> World -> Cell -> Element Message
cellHighlight cache world activeCell =
    let
        tileSizePixels =
            Render.Conversion.toPixelsValue cache.pixelsToMetersRatio Cell.size

        ( cellX, cellY ) =
            Cell.coordinates activeCell

        cellSize =
            Element.px (floor tileSizePixels)
    in
    Element.el
        [ Element.width cellSize
        , Element.height cellSize
        , Element.moveRight (toFloat (cellX - 1) * tileSizePixels)
        , Element.moveDown (toFloat (cellY - 1) * tileSizePixels)
        , Border.width borderSize.light
        , Border.rounded borderRadius.light
        , Border.solid
        , Border.color
            (highlightColor world activeCell
                |> Maybe.withDefault colors.transparent
            )
        ]
        Element.none


highlightColor : World -> Cell -> Maybe Color
highlightColor world cell =
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
