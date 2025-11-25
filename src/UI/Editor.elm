module UI.Editor exposing
    ( EditorEffect(..)
    , InputEvent
    , Model
    , Msg
    , initialModel
    , onViewportChanged
    , subscriptions
    , update
    , usingTouchDevice
    , view
    )

import BoundingBox2d exposing (BoundingBox2d)
import Browser.Events as Events
import Common exposing (GlobalCoordinates)
import Dict exposing (Dict)
import Duration exposing (Duration)
import Element exposing (Color, Element)
import Element.Background
import Element.Border as Border
import Html
import Html.Attributes
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode
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


type alias PointerInfo =
    { event : Pointer.Event
    , position : Pan.Position
    }


type alias Model =
    { activeCell : Maybe Cell
    , highlightArea : Maybe ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates )
    , longPressTimer : Maybe Duration
    , pointerDownEvent : Maybe Pointer.Event
    , lastEventDevice : Pointer.DeviceType
    , panState : Pan.PanState
    , activePointers : Dict Int PointerInfo
    , panEnabled : Bool
    }


initialModel : Model
initialModel =
    { activeCell = Nothing
    , highlightArea = Nothing
    , longPressTimer = Nothing
    , pointerDownEvent = Nothing
    , lastEventDevice = Pointer.MouseType
    , panState = Pan.init
    , activePointers = Dict.empty
    , panEnabled = True
    }


type Msg
    = OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | OverlayPointerCancel Pointer.Event
    | GlobalPointerUp
    | AnimationFrameReceived Duration
    | OverlayWheelEvent WheelEventExtended
    | NoOp


type alias WheelEventExtended =
    { mouseEvent : Mouse.Event
    , deltaX : Float
    , deltaY : Float
    , deltaMode : Wheel.DeltaMode
    }


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
    model.lastEventDevice == Pointer.TouchType


onViewportChanged : RenderCache -> Viewport -> Model -> Model
onViewportChanged cache viewport model =
    let
        canPan =
            cache.tilemapWidthPixels
                > viewport.width
                || cache.tilemapHeightPixels
                > viewport.height

        clearedPanState =
            if canPan then
                model.panState

            else
                Pan.init
    in
    { model
        | panEnabled = canPan
        , panState = clearedPanState
    }


longTapThreshold : Duration
longTapThreshold =
    Duration.milliseconds 1500


longTapIndicatorShowDelay : Duration
longTapIndicatorShowDelay =
    Duration.milliseconds 300


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
        , if model.panState.isDragging then
            Events.onMouseUp (Decode.succeed GlobalPointerUp)

          else
            Sub.none
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
                    if model.panEnabled && (abs stepResult.delta.x > 0.01 || abs stepResult.delta.y > 0.01) then
                        [ ViewportChangeRequested stepResult.delta.x stepResult.delta.y ]

                    else
                        []
            in
            ( { modelAfterLongPressTimerUpdate | panState = stepResult.state }
            , panEffect
            )

        OverlayPointerMove event ->
            let
                -- Update pointer position in cache
                updatedPointers =
                    if Dict.member event.pointerId model.activePointers then
                        Dict.update event.pointerId
                            (Maybe.map
                                (\info ->
                                    { info | position = pointerPosition event }
                                )
                            )
                            model.activePointers

                    else
                        model.activePointers

                updatedModel =
                    { model | activePointers = updatedPointers }
            in
            if model.panState.isDragging then
                ( { updatedModel | panState = Pan.updateDrag (getPanPosition event updatedModel) model.panState }
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
                        ( activateCell activeCell world updatedModel
                        , []
                        )

                    Nothing ->
                        ( updatedModel, [] )

        OverlayPointerLeave event ->
            let
                updatedModel =
                    if model.panState.isDragging then
                        { model | panState = Pan.releaseDrag model.panState }

                    else
                        model
            in
            ( updatedModel
                |> setLastEventDevice event.pointerType
                |> deactivateCell
            , []
            )

        OverlayPointerDown event ->
            let
                -- Add to active pointers cache
                pointerInfo =
                    { event = event
                    , position = pointerPosition event
                    }

                updatedPointers =
                    Dict.insert event.pointerId pointerInfo model.activePointers

                updatedModel =
                    { model | activePointers = updatedPointers }
            in
            if model.panEnabled && shouldStartPan event updatedModel then
                ( { updatedModel | panState = Pan.startDrag (getPanPosition event updatedModel) model.panState }
                , []
                )

            else if model.panState.isDragging then
                ( updatedModel, [] )

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
                        ( selectCell event eventCell cellHasRoadTile world updatedModel
                        , []
                        )

                    Nothing ->
                        ( deactivateCell updatedModel
                        , []
                        )

        OverlayPointerUp event ->
            let
                -- Remove from active pointers cache
                updatedPointers =
                    Dict.remove event.pointerId model.activePointers

                -- If panning with 2 fingers and one lifts, end pan
                shouldEndTwoFingerPan =
                    model.panState.isDragging
                        && model.lastEventDevice
                        == Pointer.TouchType
                        && Dict.size updatedPointers
                        < 2

                updatedPanState =
                    if shouldEndTwoFingerPan then
                        Pan.releaseDrag model.panState

                    else
                        model.panState
            in
            if model.panState.isDragging && isMiddleButton event then
                ( { model
                    | panState = Pan.releaseDrag model.panState
                    , activePointers = updatedPointers
                  }
                , []
                )

            else
                let
                    baseModel =
                        { model
                            | activePointers = updatedPointers
                            , panState = updatedPanState
                        }
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
                -- Clear active pointers on cancel
                clearedPointers =
                    Dict.empty

                updatedModel =
                    if model.panState.isDragging then
                        { model
                            | panState = Pan.releaseDrag model.panState
                            , activePointers = clearedPointers
                        }

                    else
                        { model | activePointers = clearedPointers }
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

        OverlayWheelEvent wheelEvent ->
            if not model.panEnabled then
                ( model, [] )

            else
                let
                    scaleFactor =
                        case wheelEvent.deltaMode of
                            Wheel.DeltaPixel ->
                                1.0

                            Wheel.DeltaLine ->
                                16.0

                            Wheel.DeltaPage ->
                                800.0

                    deltaX =
                        -wheelEvent.deltaX * scaleFactor

                    deltaY =
                        -wheelEvent.deltaY * scaleFactor

                    panState =
                        model.panState

                    updatedPanState =
                        { panState
                            | targetX = panState.targetX + deltaX
                            , targetY = panState.targetY + deltaY
                        }
                in
                ( { model | panState = updatedPanState }
                , []
                )

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


shouldStartPan : Pointer.Event -> Model -> Bool
shouldStartPan event model =
    case event.pointerType of
        Pointer.MouseType ->
            isMiddleButton event

        Pointer.TouchType ->
            Dict.size model.activePointers == 2

        Pointer.PenType ->
            isMiddleButton event


getPanPosition : Pointer.Event -> Model -> Pan.Position
getPanPosition event model =
    case event.pointerType of
        Pointer.TouchType ->
            twoFingerAveragePosition model.activePointers
                |> Maybe.withDefault (pointerPosition event)

        _ ->
            pointerPosition event


twoFingerAveragePosition : Dict Int PointerInfo -> Maybe Pan.Position
twoFingerAveragePosition pointers =
    if Dict.size pointers == 2 then
        let
            positions =
                Dict.values pointers
                    |> List.map .position

            sumX =
                List.sum (List.map .x positions)

            sumY =
                List.sum (List.map .y positions)
        in
        Just { x = sumX / 2, y = sumY / 2 }

    else
        Nothing


wheelEventDecoder : Decode.Decoder WheelEventExtended
wheelEventDecoder =
    Decode.map4 WheelEventExtended
        Mouse.eventDecoder
        (Decode.field "deltaX" Decode.float)
        (Decode.field "deltaY" Decode.float)
        (Decode.field "deltaMode" deltaModeDecoder)


deltaModeDecoder : Decode.Decoder Wheel.DeltaMode
deltaModeDecoder =
    let
        intToMode int =
            case int of
                1 ->
                    Wheel.DeltaLine

                2 ->
                    Wheel.DeltaPage

                _ ->
                    Wheel.DeltaPixel
    in
    Decode.map intToMode Decode.int


onWheelExtended : (WheelEventExtended -> msg) -> Html.Attribute msg
onWheelExtended tag =
    wheelEventDecoder
        |> Decode.map
            (\ev ->
                { message = tag ev
                , stopPropagation = False
                , preventDefault = True
                }
            )
        |> Html.Events.custom "wheel"


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
                , Element.htmlAttribute (Html.Attributes.style "touch-action" "none")
                , Element.htmlAttribute (Pointer.onMove OverlayPointerMove)
                , Element.htmlAttribute (Pointer.onLeave OverlayPointerLeave)
                , Element.htmlAttribute (Pointer.onDown OverlayPointerDown)
                , Element.htmlAttribute (Pointer.onUp OverlayPointerUp)
                , Element.htmlAttribute (Pointer.onCancel OverlayPointerCancel)
                , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
                , Element.htmlAttribute (onWheelExtended OverlayWheelEvent)
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
