module UI.UI exposing
    ( Msg
    , UIEvent(..)
    , subscriptions
    , update
    , view
    )

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes as HtmlAttribute
import Html.Events.Extra.Mouse as Mouse
import Model.Debug exposing (DebugLayerKind(..), DevOutput(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.World exposing (World)
import Render.Conversion exposing (PixelsToMetersRatio)
import UI.Core
    exposing
        ( ControlButtonContent(..)
        , ControlButtonSize(..)
        , borderRadiusButton
        , borderRadiusPanel
        , borderSize
        , colorMainBackground
        , colorMenuBackground
        , colorRenderEdge
        , containerId
        , controlButton
        , renderSafeAreaXSize
        , renderSafeAreaYSize
        , scrollbarAwareOffsetF
        , whitespaceRegular
        , whitespaceTight
        )
import UI.Editor as Editor
import UI.Model exposing (ButtonKind, UI, ZoomLevel(..))


type Msg
    = ToggleMenu
    | ZoomIn
    | ZoomOut
    | Trigger ButtonKind
    | ToggleSimulationActive Bool
    | EditorMsg Editor.Msg
    | NoOp


type UIEvent
    = GameInputReceived Editor.InputEvent
    | ZoomLevelChanged ZoomLevel
    | ButtonPressed ButtonKind


baseLayoutOptions : List Element.Option
baseLayoutOptions =
    [ Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow =
            Nothing
        }
    ]


touchLayoutOptions : List Element.Option
touchLayoutOptions =
    Element.noHover :: baseLayoutOptions


subscriptions : UI -> Sub Msg
subscriptions model =
    Sub.map EditorMsg (Editor.subscriptions model.editor)


update : World -> PixelsToMetersRatio -> Msg -> UI -> ( UI, Cmd Msg, Maybe UIEvent )
update world pixelsToMetersRatio msg model =
    case msg of
        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }
            , Cmd.none
            , Nothing
            )

        ZoomIn ->
            let
                zoomLevel =
                    zoomIn model.zoomLevel
            in
            ( { model | zoomLevel = zoomLevel }
            , Cmd.none
            , Just (ZoomLevelChanged zoomLevel)
            )

        ZoomOut ->
            let
                zoomLevel =
                    zoomOut model.zoomLevel
            in
            ( { model | zoomLevel = zoomLevel }
            , Cmd.none
            , Just (ZoomLevelChanged zoomLevel)
            )

        Trigger buttonId ->
            ( model
            , Cmd.none
            , Just (ButtonPressed buttonId)
            )

        ToggleSimulationActive isActive ->
            let
                buttonId =
                    if isActive then
                        UI.Model.ResumeSimulation

                    else
                        UI.Model.PauseSimulation
            in
            ( model
            , Cmd.none
            , Just (ButtonPressed buttonId)
            )

        EditorMsg editorMsg ->
            let
                ( editorModel, inputEvent ) =
                    Editor.update world pixelsToMetersRatio editorMsg model.editor
            in
            ( { model | editor = editorModel }
            , Cmd.none
            , inputEvent |> Maybe.map GameInputReceived
            )

        NoOp ->
            ( model, Cmd.none, Nothing )


zoomIn : ZoomLevel -> ZoomLevel
zoomIn currentLevel =
    case currentLevel of
        VeryFar ->
            Far

        Far ->
            Near

        Near ->
            Near


zoomOut : ZoomLevel -> ZoomLevel
zoomOut currentLevel =
    case currentLevel of
        Near ->
            Far

        Far ->
            VeryFar

        VeryFar ->
            VeryFar


zoomLevelToUIValue : ZoomLevel -> Float
zoomLevelToUIValue zoomLevel =
    case zoomLevel of
        VeryFar ->
            1

        Far ->
            2

        Near ->
            3


uiValueToZoomLevel : Float -> ZoomLevel
uiValueToZoomLevel value =
    case floor value of
        1 ->
            VeryFar

        2 ->
            Far

        3 ->
            Near

        _ ->
            Far


view : Liikennematto -> Element msg -> Element msg -> Html Msg
view liikennematto render renderDebugLayers =
    Element.layoutWith
        { options =
            if Editor.usingTouchDevice liikennematto.ui.editor then
                baseLayoutOptions

            else
                touchLayoutOptions
        }
        [ Background.color colorMainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        , Element.inFront (rightControls liikennematto.ui)
        , Element.inFront (leftControls liikennematto.simulationActive)
        , Element.inFront (menu liikennematto.ui)

        -- , Element.inFront (devMenu liikennematto.ui)
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
        , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
        ]
        (renderWrapper liikennematto render renderDebugLayers liikennematto.ui)


renderWrapper : Liikennematto -> Element msg -> Element msg -> UI -> Element Msg
renderWrapper { renderCache, world, screen } render debugLayers model =
    let
        renderWidth =
            floor renderCache.tilemapWidthPixels + (borderSize * 2)

        renderHeight =
            floor renderCache.tilemapHeightPixels + (borderSize * 2)

        wrapperWidth =
            renderWidth + (borderSize * 2) + renderSafeAreaXSize

        wrapperHeight =
            renderHeight + (borderSize * 2) + renderSafeAreaYSize

        horizontalAlignment =
            if wrapperWidth < screen.width then
                Element.centerX

            else
                Element.alignLeft

        ( verticalAlignment, renderTopOffset ) =
            if wrapperHeight < screen.height then
                ( Element.centerY, 0 )

            else
                ( Element.alignTop, toFloat whitespaceRegular )
    in
    Element.el
        [ Element.width (Element.px wrapperWidth)
        , Element.height (Element.px wrapperHeight)
        , horizontalAlignment
        , verticalAlignment
        ]
        (Element.el
            [ Element.width (Element.px renderWidth)
            , Element.height (Element.px renderHeight)
            , Element.htmlAttribute (HtmlAttribute.style "top" (String.fromFloat renderTopOffset ++ "px"))
            , Element.centerX
            , Element.clip
            , verticalAlignment
            , Border.solid
            , Border.rounded borderRadiusButton
            , Border.width borderSize
            , Border.color colorRenderEdge
            , Element.inFront
                (debugLayers |> Element.map (\_ -> NoOp))
            , Element.inFront
                (Editor.view
                    renderCache
                    world
                    model.editor
                    |> Element.map EditorMsg
                )
            ]
            (render |> Element.map (\_ -> NoOp))
        )


leftControls : Bool -> Element Msg
leftControls simulationActive =
    Element.row
        [ Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignLeft
        , Element.moveUp scrollbarAwareOffsetF
        , Element.moveRight scrollbarAwareOffsetF
        ]
        [ Element.el
            [ Element.padding whitespaceTight
            , Element.spacing whitespaceTight
            , Element.alignBottom
            , Background.color colorMenuBackground
            , Border.rounded borderRadiusPanel
            ]
            (simulationControl simulationActive)
        ]


rightControls : UI -> Element Msg
rightControls model =
    Element.row
        [ Element.padding whitespaceTight
        , Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignRight
        , Element.moveUp scrollbarAwareOffsetF
        , Element.moveLeft scrollbarAwareOffsetF
        , Background.color colorMenuBackground
        , Border.rounded borderRadiusPanel
        ]
        [ Element.row
            [ Element.spacing whitespaceTight
            ]
            [ UI.Core.controlButton
                { content = Icon (Icons.createIconId "new-game")
                , onPress = Trigger UI.Model.NewGame
                , selected = False
                , disabled = False
                , size = Large
                }
            ]
        ]


simulationControl : Bool -> Element Msg
simulationControl simulationActive =
    let
        ( iconKind, selected ) =
            if simulationActive then
                ( Icons.createIconId "pause", False )

            else
                ( Icons.createIconId "resume", True )
    in
    controlButton
        { content = Icon iconKind
        , onPress = ToggleSimulationActive (not simulationActive)
        , selected = selected
        , disabled = False
        , size = Large
        }


menu : UI -> Element Msg
menu model =
    if model.showMenu then
        Element.none

    else
        Element.none



-- devMenu : UI -> Element Msg
-- devMenu model =
--     if model.debug.showDevMenu then
--         DebugPanel.devMenu model
--     else
--         Element.none
