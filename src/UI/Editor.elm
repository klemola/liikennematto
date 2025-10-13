module UI.Editor exposing
    ( InputEvent
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
import Length exposing (Meters)
import Model.RenderCache exposing (RenderCache)
import Model.World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio, toPixelsValue)
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
import UI.TimerIndicator


type alias Model =
    { activeCell : Maybe Cell
    , highlightArea : Maybe ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates )
    , longPressTimer : Maybe Duration
    , pointerDownEvent : Maybe Pointer.Event
    , lastEventDevice : Pointer.DeviceType
    }


initialModel : Model
initialModel =
    { activeCell = Nothing
    , highlightArea = Nothing
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


type alias InputEvent =
    { cell : Cell
    , kind : InputKind
    }


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
    Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)



-- Update


update : World -> PixelsToMetersRatio -> Msg -> Model -> ( Model, Maybe InputEvent )
update world pixelsToMetersRatio msg model =
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
                    pixelsToMetersRatio
                    tilemapConfig
                    event
            of
                Just activeCell ->
                    ( activateCell activeCell world model
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
            case pointerEventToCell pixelsToMetersRatio tilemapConfig event of
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
            case pointerEventToCell pixelsToMetersRatio tilemapConfig event of
                Just pointerUpCell ->
                    let
                        pointerDownCell =
                            model.pointerDownEvent
                                |> Maybe.andThen
                                    (pointerEventToCell pixelsToMetersRatio tilemapConfig)

                        inputEvent =
                            resolvePointerUp
                                pointerDownCell
                                pointerUpCell
                                event
                                model
                    in
                    ( activateCell pointerUpCell world baseModel
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


pointerEventToCell : PixelsToMetersRatio -> TilemapConfig -> Pointer.Event -> Maybe Cell
pointerEventToCell pixelsToMetersRatio constraints event =
    let
        ( overlayX, overlayY ) =
            event.pointer.offsetPos

        cellMetersValue =
            Quantity.unwrap Cell.size

        overlayXMetersValue =
            overlayX
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
                |> Quantity.unwrap

        overlayYMetersValue =
            overlayY
                |> Render.Conversion.toMetersValue pixelsToMetersRatio
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


view : RenderCache -> World -> Model -> Element Msg
view cache world model =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Html.Attributes.id overlayId)
        , Element.inFront
            (case model.highlightArea of
                Just area ->
                    highlightAreaView cache area

                Nothing ->
                    Element.none
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


cellHighlight : RenderCache -> World -> Cell -> Element Msg
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


highlightAreaView : RenderCache -> ( Point2d Meters GlobalCoordinates, BoundingBox2d Meters GlobalCoordinates ) -> Element msg
highlightAreaView cache ( origin, area ) =
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
        , Element.moveRight x
        , Element.moveDown (cache.tilemapHeightPixels - y)
        , Border.width cellHighlightWidth
        , Border.rounded targetRadius
        , Border.solid
        , Border.color UI.Core.colorDanger
        , Element.Background.color UI.Core.colorDanger
        ]
        Element.none
