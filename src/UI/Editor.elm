module UI.Editor exposing
    ( Model
    , Msg
    , initialModel
    , subscriptions
    , update
    , usingTouchDevice
    , view
    )

import Browser.Events as Events
import Duration exposing (Duration)
import Element exposing (Color, Element)
import Element.Border as Border
import Html.Attributes
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Pointer as Pointer
import Model.RenderCache exposing (RenderCache)
import Model.World as World exposing (World)
import Quantity
import Render.Conversion
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( TilemapConfig
        , canBuildRoadAt
        , cellHasFixedTile
        , getTilemapConfig
        )
import UI.Core
    exposing
        ( InputEvent
        , InputKind(..)
        , borderRadiusButton
        , cellHighlightWidth
        , colorTransparent
        , overlayId
        )
import UI.TimerIndicator


type alias Model =
    { activeCell : Maybe Cell
    , longPressTimer : Maybe Duration
    , pointerDownEvent : Maybe Pointer.Event
    , lastEventDevice : Pointer.DeviceType
    }


initialModel : Model
initialModel =
    { activeCell = Nothing
    , longPressTimer = Nothing
    , pointerDownEvent = Nothing
    , lastEventDevice = Pointer.MouseType
    }


type Msg
    = OverlayPointerMove Pointer.Event
    | OverlayPointerLeave Pointer.Event
    | OverlayPointerDown Pointer.Event
    | OverlayPointerUp Pointer.Event
    | OverlayPointerCancel Pointer.Event
    | AnimationFrameReceived Duration
    | NoOp


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
    Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)



-- Update


update : World -> RenderCache msg -> Msg -> Model -> ( Model, Maybe InputEvent )
update world renderCache msg model =
    let
        tilemapConfig =
            getTilemapConfig world.tilemap
    in
    case msg of
        AnimationFrameReceived delta ->
            ( advanceLongPressTimer delta model
            , Nothing
            )

        OverlayPointerMove event ->
            case
                pointerEventToCell
                    renderCache
                    tilemapConfig
                    event
            of
                Just activeCell ->
                    ( activateCell activeCell model
                    , Nothing
                    )

                Nothing ->
                    ( model, Nothing )

        OverlayPointerLeave event ->
            ( model
                |> setLastEventDevice event.pointerType
                |> deactivateCell
            , Nothing
            )

        OverlayPointerDown event ->
            case pointerEventToCell renderCache tilemapConfig event of
                Just eventCell ->
                    let
                        cellHasTile =
                            cellHasFixedTile eventCell world.tilemap
                    in
                    ( selectCell event eventCell cellHasTile model
                    , Nothing
                    )

                Nothing ->
                    ( deactivateCell model
                    , Nothing
                    )

        OverlayPointerUp event ->
            let
                baseModel =
                    model
                        |> clearPointerDownEvent
                        |> resetLongPressTimer
            in
            case pointerEventToCell renderCache tilemapConfig event of
                Just pointerUpCell ->
                    let
                        pointerDownCell =
                            model.pointerDownEvent |> Maybe.andThen (pointerEventToCell renderCache tilemapConfig)

                        inputEvent =
                            resolvePointerUp
                                pointerDownCell
                                pointerUpCell
                                event
                                model
                    in
                    ( activateCell pointerUpCell baseModel
                    , inputEvent
                    )

                Nothing ->
                    ( baseModel, Nothing )

        OverlayPointerCancel event ->
            ( model
                |> clearPointerDownEvent
                |> resetLongPressTimer
                |> deactivateCell
                |> setLastEventDevice event.pointerType
            , Nothing
            )

        NoOp ->
            ( model, Nothing )


activateCell : Cell -> Model -> Model
activateCell cell model =
    if
        -- Cell already active?
        model.activeCell
            |> Maybe.map (Cell.identical cell)
            |> Maybe.withDefault False
    then
        model

    else
        { model | activeCell = Just cell }


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


selectCell : Pointer.Event -> Cell -> Bool -> Model -> Model
selectCell event eventCell hasTile initialEditor =
    initialEditor
        |> setLastEventDevice event.pointerType
        |> storePointerDownEvent event
        |> activateCell eventCell
        |> (\editor ->
                if event.pointerType == Pointer.MouseType || not hasTile then
                    editor

                else
                    { editor | longPressTimer = Just Quantity.zero }
           )


pointerEventToCell : RenderCache msg -> TilemapConfig -> Pointer.Event -> Maybe Cell
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


resolvePointerUp : Maybe Cell -> Cell -> Pointer.Event -> Model -> Maybe InputEvent
resolvePointerUp pointerDownCell pointerUpCell pointerUpEvent model =
    let
        validRelease =
            pointerDownCell
                |> Maybe.map (Cell.identical pointerUpCell)
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


view : RenderCache msg -> World -> Model -> Element Msg
view cache world model =
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
        (case model.activeCell of
            Just cell ->
                if model.lastEventDevice == Pointer.MouseType then
                    cellHighlight cache world cell

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


cellHighlight : RenderCache msg -> World -> Cell -> Element Msg
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
            canBuildRoadAt cell world.tilemap

        mightDestroyLot =
            World.hasLot cell world
    in
    if canBuildHere && mightDestroyLot then
        Just UI.Core.colorDanger

    else if canBuildHere then
        Just UI.Core.colorTarget

    else
        Just UI.Core.colorNotAllowed
