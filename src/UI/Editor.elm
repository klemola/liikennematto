module UI.Editor exposing
    ( EditorEffect(..)
    , InputEvent
    , Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , usingTouchDevice
    , view
    )

import BoundingBox2d exposing (BoundingBox2d)
import Browser.Events as Events
import Common exposing (GlobalCoordinates)
import Duration exposing (Duration)
import Element exposing (Color, Element)
import Element.Background
import Element.Border as Border
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Json.Decode
import Length exposing (Meters)
import Model.RenderCache exposing (RenderCache)
import Model.World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio, toPixelsValue)
import Render.Viewport exposing (Viewport)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( TilemapConfig
        , cellSupportsRoadPlacement
        , fixedTileByCell
        , getTilemapConfig
        , largeTileBounds
        , roadTileFromCell
        )
import UI.Core
    exposing
        ( InputKind(..)
        , cellHighlightWidth
        , colorTransparent
        , overlayId
        )
import UI.Pan as Pan
import UI.TimerIndicator


type alias Model =
    { activeCell : Maybe Cell
    , highlightArea : Maybe ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates )
    , longPressTimer : Maybe Duration
    , pointerDownEvent : Maybe Pointer.Event
    , lastEventDevice : Pointer.DeviceType
    , panState : Pan.PanState
    }


initialModel : Model
initialModel =
    { activeCell = Nothing
    , highlightArea = Nothing
    , longPressTimer = Nothing
    , pointerDownEvent = Nothing
    , lastEventDevice = Pointer.MouseType
    , panState = Pan.init
    }


type Msg
    = OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | OverlayPointerCancel Pointer.Event
    | GlobalPointerUp
    | AnimationFrameReceived Duration
    | NoOp


type alias InputEvent =
    { cell : Cell
    , kind : InputKind
    }


type EditorEffect
    = GameInput InputEvent
    | ViewportChangeRequested Float Float


targetRadius : Int
targetRadius =
    10


usingTouchDevice : Model -> Bool
usingTouchDevice model =
    model.lastEventDevice == Pointer.MouseType


longTapThreshold : Duration
longTapThreshold =
    Duration.milliseconds 1500


longTapIndicatorShowDelay : Duration
longTapIndicatorShowDelay =
    Duration.milliseconds 300


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
        , Events.onMouseUp (Json.Decode.succeed GlobalPointerUp)
        ]



-- Update


update : World -> PixelsToMetersRatio -> Viewport -> Msg -> Model -> ( Model, List EditorEffect )
update world pixelsToMetersRatio viewport msg model =
    let
        tilemapConfig =
            getTilemapConfig world.tilemap
    in
    case msg of
        AnimationFrameReceived delta ->
            let
                modelAfterLongPressTimerUpdate =
                    advanceLongPressTimer delta model

                stepResult =
                    Pan.step delta modelAfterLongPressTimerUpdate.panState

                panEffect =
                    if abs stepResult.delta.x > 0.01 || abs stepResult.delta.y > 0.01 then
                        [ ViewportChangeRequested stepResult.delta.x stepResult.delta.y ]

                    else
                        []
            in
            ( { modelAfterLongPressTimerUpdate | panState = stepResult.state }
            , panEffect
            )

        OverlayPointerMove event ->
            if model.panState.isDragging then
                ( { model | panState = Pan.updateDrag (pointerPosition event) model.panState }
                , []
                )

            else
                case
                    pointerEventToCell
                        pixelsToMetersRatio
                        viewport
                        tilemapConfig
                        event
                of
                    Just activeCell ->
                        ( activateCell activeCell world model
                        , []
                        )

                    Nothing ->
                        ( model, [] )

        OverlayPointerLeave event ->
            ( model
                |> setLastEventDevice event.pointerType
                |> deactivateCell
            , []
            )

        OverlayPointerDown event ->
            if isMiddleButton event then
                ( { model | panState = Pan.startDrag (pointerPosition event) model.panState }
                , []
                )

            else if model.panState.isDragging then
                ( model, [] )

            else
                case pointerEventToCell pixelsToMetersRatio viewport tilemapConfig event of
                    Just eventCell ->
                        let
                            cellHasRoadTile =
                                case roadTileFromCell eventCell world.tilemap of
                                    Just _ ->
                                        True

                                    Nothing ->
                                        False
                        in
                        ( selectCell event eventCell cellHasRoadTile world model
                        , []
                        )

                    Nothing ->
                        ( deactivateCell model
                        , []
                        )

        OverlayPointerUp event ->
            if model.panState.isDragging && isMiddleButton event then
                ( { model | panState = Pan.releaseDrag model.panState }
                , []
                )

            else
                let
                    baseModel =
                        model
                            |> clearPointerDownEvent
                            |> resetLongPressTimer
                in
                if model.panState.isDragging then
                    ( baseModel, [] )

                else
                    case pointerEventToCell pixelsToMetersRatio viewport tilemapConfig event of
                        Just pointerUpCell ->
                            let
                                pointerDownCell =
                                    model.pointerDownEvent
                                        |> Maybe.andThen
                                            (pointerEventToCell pixelsToMetersRatio viewport tilemapConfig)

                                inputEvent =
                                    resolvePointerUp
                                        pointerDownCell
                                        pointerUpCell
                                        event
                                        model
                            in
                            ( activateCell pointerUpCell world baseModel
                            , inputEvent
                                |> Maybe.map GameInput
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                            )

                        Nothing ->
                            ( baseModel, [] )

        OverlayPointerCancel event ->
            let
                updatedModel =
                    if model.panState.isDragging then
                        { model | panState = Pan.releaseDrag model.panState }

                    else
                        model
            in
            ( updatedModel
                |> clearPointerDownEvent
                |> resetLongPressTimer
                |> deactivateCell
                |> setLastEventDevice event.pointerType
            , []
            )

        GlobalPointerUp ->
            if model.panState.isDragging then
                ( { model | panState = Pan.releaseDrag model.panState }
                , []
                )

            else
                ( model, [] )

        NoOp ->
            ( model, [] )


activateCell : Cell -> World -> Model -> Model
activateCell cell world model =
    if
        -- Cell already active?
        model.activeCell
            |> Maybe.map (Cell.isIdentical cell)
            |> Maybe.withDefault False
    then
        model

    else
        { model
            | activeCell = Just cell
            , highlightArea =
                Maybe.andThen
                    (\tile -> largeTileBounds cell tile world.tilemap)
                    (fixedTileByCell world.tilemap cell)
        }


deactivateCell : Model -> Model
deactivateCell model =
    { model | activeCell = Nothing }


storePointerDownEvent : Pointer.Event -> Model -> Model
storePointerDownEvent event model =
    { model | pointerDownEvent = Just event }


clearPointerDownEvent : Model -> Model
clearPointerDownEvent model =
    { model | pointerDownEvent = Nothing }


resetLongPressTimer : Model -> Model
resetLongPressTimer model =
    { model | longPressTimer = Nothing }


advanceLongPressTimer : Duration -> Model -> Model
advanceLongPressTimer delta model =
    { model
        | longPressTimer = model.longPressTimer |> Maybe.map (Quantity.plus delta)
    }


setLastEventDevice : Pointer.DeviceType -> Model -> Model
setLastEventDevice deviceType model =
    { model | lastEventDevice = deviceType }


isMiddleButton : Pointer.Event -> Bool
isMiddleButton event =
    event.pointer.button == Mouse.MiddleButton


pointerPosition : Pointer.Event -> Pan.Position
pointerPosition event =
    let
        ( x, y ) =
            event.pointer.offsetPos
    in
    { x = x, y = y }


selectCell : Pointer.Event -> Cell -> Bool -> World -> Model -> Model
selectCell event eventCell hasRoadTile world initialEditor =
    initialEditor
        |> setLastEventDevice event.pointerType
        |> storePointerDownEvent event
        |> activateCell eventCell world
        |> (\editor ->
                if event.pointerType == Pointer.MouseType || not hasRoadTile then
                    editor

                else
                    { editor | longPressTimer = Just Quantity.zero }
           )


pointerEventToCell : PixelsToMetersRatio -> Viewport -> TilemapConfig -> Pointer.Event -> Maybe Cell
pointerEventToCell pixelsToMetersRatio viewport constraints event =
    let
        ( overlayX, overlayY ) =
            event.pointer.offsetPos

        cellMetersValue =
            Quantity.unwrap Cell.size

        viewportOffsetXMeters =
            viewport.x
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
                |> Quantity.unwrap

        viewportOffsetYMeters =
            viewport.y
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
                |> Quantity.unwrap

        overlayXMetersValue =
            overlayX
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
                |> Quantity.unwrap

        overlayYMetersValue =
            overlayY
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
                |> Quantity.unwrap

        worldX =
            overlayXMetersValue + viewportOffsetXMeters

        worldY =
            overlayYMetersValue + viewportOffsetYMeters

        coordinates =
            ( 1 + floor (worldX / cellMetersValue)
            , 1 + floor (worldY / cellMetersValue)
            )
    in
    Cell.fromCoordinates constraints coordinates


resolvePointerUp : Maybe Cell -> Cell -> Pointer.Event -> Model -> Maybe InputEvent
resolvePointerUp pointerDownCell pointerUpCell pointerUpEvent model =
    let
        validRelease =
            pointerDownCell
                |> Maybe.map (Cell.isIdentical pointerUpCell)
                |> Maybe.withDefault False

        isRightClick =
            pointerUpEvent.pointer.button == Mouse.SecondButton

        isLongTap =
            case model.longPressTimer of
                Just elapsed ->
                    validRelease && (elapsed |> Quantity.greaterThanOrEqualTo longTapThreshold)

                Nothing ->
                    False
    in
    if isLongTap || isRightClick then
        Just { cell = pointerUpCell, kind = Secondary }

    else if validRelease then
        Just { cell = pointerUpCell, kind = Primary }

    else
        Nothing



-- Views


view : RenderCache -> Viewport -> World -> Model -> Element Msg
view cache viewport world model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Html.Attributes.id overlayId)
        , Element.inFront
            (Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clip
                ]
                (Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.inFront
                        (case model.highlightArea of
                            Just area ->
                                highlightAreaView cache viewport area

                            Nothing ->
                                Element.none
                        )
                    ]
                    (case model.activeCell of
                        Just cell ->
                            if model.lastEventDevice == Pointer.MouseType then
                                cellHighlight cache viewport world cell

                            else
                                case model.longPressTimer of
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
                )
            )
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
        Element.none


cellHighlight : RenderCache -> Viewport -> World -> Cell -> Element Msg
cellHighlight cache viewport world activeCell =
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
        , Element.moveRight (toFloat (cellX - 1) * tileSizePixels - viewport.x)
        , Element.moveDown (toFloat (cellY - 1) * tileSizePixels - viewport.y)
        , Border.width cellHighlightWidth
        , Border.rounded targetRadius
        , Border.solid
        , Border.color
            (highlightColor world activeCell
                |> Maybe.withDefault colorTransparent
            )
        ]
        Element.none


highlightColor : World -> Cell -> Maybe Color
highlightColor world cell =
    if cellSupportsRoadPlacement cell world.tilemap then
        Just UI.Core.colorTarget

    else
        Just UI.Core.colorNotAllowed


highlightAreaView : RenderCache -> Viewport -> ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates ) -> Element msg
highlightAreaView cache viewport ( origin, area ) =
    let
        { x, y } =
            Point2d.toRecord (toPixelsValue cache.pixelsToMetersRatio) origin

        ( width, height ) =
            BoundingBox2d.dimensions area

        widthPixels =
            Element.px (floor (toPixelsValue cache.pixelsToMetersRatio width))

        heightPixels =
            Element.px (floor (toPixelsValue cache.pixelsToMetersRatio height))
    in
    Element.el
        [ Element.width widthPixels
        , Element.height heightPixels
        , Element.moveRight (x - viewport.x)
        , Element.moveDown (cache.tilemapHeightPixels - y - viewport.y)
        , Border.width cellHighlightWidth
        , Border.rounded targetRadius
        , Border.solid
        , Border.color UI.Core.colorDanger
        , Element.Background.color UI.Core.colorDanger
        ]
        Element.none
