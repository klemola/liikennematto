module UI.Editor exposing
    ( overlay
    , update
    , zoomControl
    )

import Audio exposing (playSound)
import Browser.Dom
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
import Model.RenderCache as RenderCache exposing (RenderCache, refreshTilemapCache, setTilemapCache)
import Model.Tile as Tile
import Model.Tilemap as Tilemap exposing (TilemapUpdateResult)
import Model.World as World exposing (World)
import Quantity
import Render.Conversion
import Simulation.Zoning as Zoning
import Task
import UI.Core
    exposing
        ( borderRadiusButton
        , borderSize
        , cellHighlightWidth
        , colorBorder
        , colorMenuBackground
        , colorTransparent
        , colorZoomStepGuide
        , colorZoomThumbBackground
        , colorZoomTrackBackground
        , containerId
        , overlayId
        , renderSafeAreaYSize
        , whitespaceRegular
        , whitespaceTight
        , zoomControlWidth
        , zoomTrackHeight
        , zoomTrackWidth
        )
import UI.TimerIndicator


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
        InGame ->
            ( model, centerView )

        GameSetupComplete ->
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
                        |> Zoning.removeInvalidLots tilemapUpdateResult.transitionedCells

                ( nextRenderCache, dynamicTiles ) =
                    refreshTilemapCache tilemapUpdateResult renderCache

                ( nextEditor, tilemapChangedEffects ) =
                    resolveTilemapUpdate delta tilemapUpdateResult model
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
                , dynamicTiles = dynamicTiles
                , editor = nextEditor
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds tilemapUpdateResult.actions)
            )

        SpawnTestCar ->
            -- TODO: delay event
            ( { model
                | world =
                    World.addEvent
                        World.SpawnTestCar
                        model.time
                        model.world
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

        OverlayPointerCancel event ->
            ( { model
                | editor =
                    editor
                        |> Editor.clearPointerDownEvent
                        |> Editor.resetLongPressTimer
                        |> Editor.deactivateCell
                        |> Editor.setLastEventDevice event.pointerType
              }
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
                    playSound sound
        )


centerView : Cmd Message
centerView =
    Browser.Dom.getViewportOf containerId
        |> Task.andThen
            (\domViewport ->
                Browser.Dom.setViewportOf
                    containerId
                    ((domViewport.scene.width - domViewport.viewport.width) / 2)
                    ((domViewport.scene.height - domViewport.viewport.height - toFloat renderSafeAreaYSize) / 2)
            )
        |> Task.attempt (\_ -> NoOp)


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
                    if List.isEmpty tilemapUpdateResult.transitionedCells then
                        editor

                    else
                        Editor.createPendingTilemapChange tilemapUpdateResult.transitionedCells editor
            in
            ( nextEditor
            , Cmd.none
            )

        Just pendingTilemapChange ->
            let
                ( changeTimer, currentChangedCells ) =
                    pendingTilemapChange

                nextTimer =
                    if not (List.isEmpty tilemapUpdateResult.transitionedCells) then
                        -- The tilemap changed during the delay, reset it (AKA debounce)
                        Editor.minTilemapChangeFrequency

                    else
                        changeTimer
                            |> Quantity.minus delta
                            |> Quantity.max Quantity.zero

                nextChangedCells =
                    Editor.combineChangedCells tilemapUpdateResult.transitionedCells currentChangedCells
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
            Tilemap.removeTile cell world.tilemap

        nextWorld =
            { world | tilemap = nextTilemap }
    in
    ( { model
        | world = nextWorld
        , editor = Editor.activateCell cell model.editor
        , renderCache = setTilemapCache nextTilemap renderCache
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
                , Element.htmlAttribute (Pointer.onCancel OverlayPointerCancel)
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
        , Border.width cellHighlightWidth
        , Border.rounded borderRadiusButton
        , Border.solid
        , Border.color
            (highlightColor world activeCell
                |> Maybe.withDefault colorTransparent
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
        Just UI.Core.colorDanger

    else if canBuildHere then
        Just UI.Core.colorTarget

    else
        Just UI.Core.colorNotAllowed


zoomControl : Editor -> Element Message
zoomControl editor =
    let
        baseWidth =
            zoomControlWidth

        baseHeight =
            zoomTrackHeight

        paddingX =
            whitespaceTight

        paddingY =
            whitespaceTight

        sliderWidth =
            baseWidth - (2 * paddingX)

        sliderHeight =
            baseHeight - (2 * paddingY)

        thumbWidth =
            zoomTrackWidth + (2 * paddingX)

        thumbHeight =
            zoomTrackWidth
    in
    Element.el
        [ Element.paddingXY paddingX paddingY
        , Element.width (Element.px baseWidth)
        , Element.height (Element.px baseHeight)
        , Element.alignLeft
        , Element.alignBottom
        , Background.color colorMenuBackground
        , Border.rounded borderRadiusButton
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
                    , Background.color colorZoomThumbBackground
                    , Border.rounded borderRadiusButton
                    , Border.solid
                    , Border.width borderSize
                    , Border.color colorBorder
                    ]
            }
        )


track : Element Message
track =
    let
        stepGuide =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px whitespaceRegular)
                , Background.color colorZoomStepGuide
                ]
                Element.none
    in
    Element.el
        [ Element.width (Element.px zoomTrackWidth)
        , Element.height Element.fill
        , Element.centerX
        , Element.clip
        , Background.color colorZoomTrackBackground
        , Border.solid
        , Border.width borderSize
        , Border.color colorBorder
        , Border.rounded borderRadiusButton
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
