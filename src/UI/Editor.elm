module UI.Editor exposing
    ( EditorEffect(..)
    , InputEvent
    , Model
    , Msg
    , PointerInfo
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
import Model.Screen exposing (Screen)
import Model.World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion exposing (defaultPixelsToMetersRatio, toPixelsValue)
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
    | OverlayWheelEvent WheelEventExtended
    | GlobalPointerUp
    | AnimationFrameReceived Duration
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
    | ViewportChangeRequested Float Float Bool


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
            (cache.tilemapWidthPixels > viewport.width)
                || (cache.tilemapHeightPixels > viewport.height)

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


update : World -> Viewport -> Screen -> Msg -> Model -> ( Model, List EditorEffect )
update world viewport screen msg model =
    let
        tilemapConfig =
            getTilemapConfig world.tilemap
    in
    case msg of
        AnimationFrameReceived delta ->
            let
                modelAfterLongPressTimerUpdate =
                    if model.panState.isDragging then
                        model

                    else
                        advanceLongPressTimer delta model

                stepResult =
                    Pan.step delta modelAfterLongPressTimerUpdate.panState

                shouldSnap =
                    abs stepResult.state.velocityX < 0.5 && abs stepResult.state.velocityY < 0.5

                panEffect =
                    if model.panEnabled && (abs stepResult.delta.x > 0.001 || abs stepResult.delta.y > 0.001 || shouldSnap) then
                        [ ViewportChangeRequested stepResult.delta.x stepResult.delta.y shouldSnap ]

                    else
                        []
            in
            ( { modelAfterLongPressTimerUpdate | panState = stepResult.state }
            , panEffect
            )

        OverlayPointerMove event ->
            let
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
                        viewport
                        screen
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
                    |> resetLongPressTimer
                    |> clearHighlightArea
                , []
                )

            else if model.panState.isDragging then
                ( updatedModel, [] )

            else
                case pointerEventToCell viewport screen tilemapConfig event of
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
                updatedPointers =
                    Dict.remove event.pointerId model.activePointers
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
                    shouldEndTwoFingerPan =
                        model.panState.isDragging
                            && (model.lastEventDevice == Pointer.TouchType)
                            && (Dict.size updatedPointers < 2)

                    updatedPanState =
                        if shouldEndTwoFingerPan then
                            Pan.releaseDrag model.panState

                        else
                            model.panState

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
                    case pointerEventToCell viewport screen tilemapConfig event of
                        Just pointerUpCell ->
                            let
                                pointerDownCell =
                                    model.pointerDownEvent
                                        |> Maybe.andThen
                                            (pointerEventToCell viewport screen tilemapConfig)

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
                                -- Ignore massive delta
                                0

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


clearHighlightArea : Model -> Model
clearHighlightArea model =
    { model | highlightArea = Nothing }


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


pointerEventToCell : Viewport -> Screen -> TilemapConfig -> Pointer.Event -> Maybe Cell
pointerEventToCell viewport screen constraints event =
    let
        ( screenX, screenY ) =
            event.pointer.offsetPos

        -- Scale screen coordinates to viewBox coordinates
        scaleX =
            viewport.width / toFloat screen.width

        scaleY =
            viewport.height / toFloat screen.height

        viewBoxX =
            screenX * scaleX

        viewBoxY =
            screenY * scaleY

        cellMetersValue =
            Length.inMeters Cell.size

        viewportOffsetXMeters =
            viewport.x
                |> Render.Conversion.toMetersValue defaultPixelsToMetersRatio
                |> Length.inMeters

        viewportOffsetYMeters =
            viewport.y
                |> Render.Conversion.toMetersValue defaultPixelsToMetersRatio
                |> Length.inMeters

        viewBoxXMeters =
            viewBoxX
                |> Render.Conversion.toMetersValue defaultPixelsToMetersRatio
                |> Length.inMeters

        viewBoxYMeters =
            viewBoxY
                |> Render.Conversion.toMetersValue defaultPixelsToMetersRatio
                |> Length.inMeters

        worldX =
            viewBoxXMeters + viewportOffsetXMeters

        worldY =
            viewBoxYMeters + viewportOffsetYMeters

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


view : RenderCache -> Viewport -> Screen -> World -> Model -> Element Msg
view cache viewport screen world model =
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
                                highlightAreaView cache viewport screen area

                            Nothing ->
                                Element.none
                        )
                    ]
                    (case model.activeCell of
                        Just cell ->
                            if model.lastEventDevice == Pointer.MouseType && not model.panState.isDragging then
                                cellHighlight viewport screen world cell

                            else
                                case model.longPressTimer of
                                    Just elapsed ->
                                        UI.TimerIndicator.view
                                            longTapThreshold
                                            longTapIndicatorShowDelay
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
                , Element.htmlAttribute
                    (Html.Attributes.style "cursor"
                        (if model.panState.isDragging then
                            "grabbing"

                         else
                            "auto"
                        )
                    )
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


cellHighlight : Viewport -> Screen -> World -> Cell -> Element Msg
cellHighlight viewport screen world activeCell =
    let
        -- Scale factor from viewBox-space to screen-space
        scaleX =
            toFloat screen.width / viewport.width

        scaleY =
            toFloat screen.height / viewport.height

        tileSizePixels =
            Render.Conversion.toPixelsValue defaultPixelsToMetersRatio Cell.size

        ( cellX, cellY ) =
            Cell.coordinates activeCell

        -- Position in viewBox coordinates
        viewBoxX =
            toFloat (cellX - 1) * tileSizePixels - viewport.x

        viewBoxY =
            toFloat (cellY - 1) * tileSizePixels - viewport.y

        -- Scale to screen coordinates
        screenX =
            viewBoxX * scaleX

        screenY =
            viewBoxY * scaleY

        screenTileSize =
            tileSizePixels * scaleX
    in
    Element.el
        [ Element.width (Element.px (floor screenTileSize))
        , Element.height (Element.px (floor screenTileSize))
        , Element.moveRight screenX
        , Element.moveDown screenY
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


highlightAreaView : RenderCache -> Viewport -> Screen -> ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates ) -> Element msg
highlightAreaView cache viewport screen ( origin, area ) =
    let
        -- Scale factor from viewBox-space to screen-space
        scaleX =
            toFloat screen.width / viewport.width

        scaleY =
            toFloat screen.height / viewport.height

        { x, y } =
            Point2d.toRecord (toPixelsValue defaultPixelsToMetersRatio) origin

        ( width, height ) =
            BoundingBox2d.dimensions area

        -- Position in viewBox coordinates
        viewBoxX =
            x - viewport.x

        viewBoxY =
            cache.tilemapHeightPixels - y - viewport.y

        -- Scale to screen coordinates
        screenX =
            viewBoxX * scaleX

        screenY =
            viewBoxY * scaleY

        screenWidth =
            toPixelsValue defaultPixelsToMetersRatio width * scaleX

        screenHeight =
            toPixelsValue defaultPixelsToMetersRatio height * scaleY
    in
    Element.el
        [ Element.width (Element.px (floor screenWidth))
        , Element.height (Element.px (floor screenHeight))
        , Element.moveRight screenX
        , Element.moveDown screenY
        , Border.width cellHighlightWidth
        , Border.rounded targetRadius
        , Border.solid
        , Border.color UI.Core.colorDanger
        , Element.Background.color UI.Core.colorDanger
        ]
        Element.none
