module UI.UI exposing (subscriptions, update, view)

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes as HtmlAttribute
import Html.Events.Extra.Mouse as Mouse
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto)
import Model.World exposing (World)
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
import UI.DebugPanel as DebugPanel
import UI.Editor as Editor
import UI.ZoomControl as ZoomControl


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


subscriptions : Liikennematto -> Sub Message
subscriptions model =
    Sub.map EditorMsg (Editor.subscriptions model.editor)


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        EditorMsg editorMsg ->
            let
                ( editorModel, inputEvent ) =
                    Editor.update model.world model.renderCache editorMsg model.editor
            in
            ( { model | editor = editorModel }
            , inputEvent
                |> Maybe.map (InputReceived >> Message.asCmd)
                |> Maybe.withDefault Cmd.none
            )

        ZoomControlMsg zoomControlMsg ->
            let
                ( zoomControlModel, zoomLevel ) =
                    ZoomControl.update zoomControlMsg model.zoomControl
            in
            ( { model | zoomControl = zoomControlModel }
            , Message.asCmd (ZoomLevelChanged zoomLevel)
            )

        _ ->
            DebugPanel.update msg model


view : Liikennematto -> Element Message -> Element Message -> Html Message
view model render renderDebugLayers =
    Element.layoutWith
        { options =
            if Editor.usingTouchDevice model.editor then
                baseLayoutOptions

            else
                touchLayoutOptions
        }
        [ Background.color colorMainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        , Element.inFront (rightControls model)
        , Element.inFront (leftControls model)
        , Element.inFront (debugPanel model)
        , Element.inFront (devMenu model)
        , Element.inFront (restoreGameControl model.previousWorld)
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
        , Element.htmlAttribute (Mouse.onContextMenu (\_ -> NoOp))
        ]
        (renderWrapper model render renderDebugLayers)


renderWrapper : Liikennematto -> Element Message -> Element Message -> Element Message
renderWrapper model render debugLayers =
    let
        renderWidth =
            floor model.renderCache.tilemapWidthPixels + (borderSize * 2)

        renderHeight =
            floor model.renderCache.tilemapHeightPixels + (borderSize * 2)

        wrapperWidth =
            renderWidth + (borderSize * 2) + renderSafeAreaXSize

        wrapperHeight =
            renderHeight + (borderSize * 2) + renderSafeAreaYSize

        horizontalAlignment =
            if wrapperWidth < model.screen.width then
                Element.centerX

            else
                Element.alignLeft

        ( verticalAlignment, renderTopOffset ) =
            if wrapperHeight < model.screen.height then
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
            , Element.inFront debugLayers
            , Element.inFront
                (Editor.view
                    model.renderCache
                    model.world
                    model.editor
                    |> Element.map EditorMsg
                )
            ]
            render
        )


leftControls : Liikennematto -> Element Message
leftControls model =
    Element.row
        [ Element.spacing whitespaceTight
        , Element.alignBottom
        , Element.alignLeft
        , Element.moveUp scrollbarAwareOffsetF
        , Element.moveRight scrollbarAwareOffsetF
        ]
        [ ZoomControl.view model.zoomControl
            |> Element.map ZoomControlMsg
        , Element.el
            [ Element.padding whitespaceTight
            , Element.spacing whitespaceTight
            , Element.alignBottom
            , Background.color colorMenuBackground
            , Border.rounded borderRadiusPanel
            ]
            (simulationControl model.simulationActive)
        ]


rightControls : Liikennematto -> Element Message
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
                { content = Icon Icons.NewGame
                , onPress = NewGame
                , selected = False
                , disabled = False
                , size = Large
                }
            , UI.Core.controlButton
                { content = Icon Icons.DebugPanel
                , onPress = ToggleDebugPanel
                , selected = model.debug.showDebugPanel
                , disabled = False
                , size = Large
                }
            ]
        ]


simulationControl : Bool -> Element Message
simulationControl simulationActive =
    let
        ( iconKind, selected ) =
            if simulationActive then
                ( Icons.Pause, False )

            else
                ( Icons.Resume, True )
    in
    controlButton
        { content = Icon iconKind
        , onPress = ToggleSimulationActive
        , selected = selected
        , disabled = False
        , size = Large
        }


debugPanel : Liikennematto -> Element Message
debugPanel model =
    if model.debug.showDebugPanel then
        DebugPanel.view model

    else
        Element.none


devMenu : Liikennematto -> Element Message
devMenu model =
    if model.debug.showDevMenu then
        DebugPanel.devMenu model

    else
        Element.none


restoreGameControl : Maybe World -> Element Message
restoreGameControl previousWorld =
    case previousWorld of
        Just _ ->
            Element.el
                [ Element.padding whitespaceTight
                , Element.spacing whitespaceTight
                , Element.alignTop
                , Element.alignLeft
                , Element.moveDown scrollbarAwareOffsetF
                , Element.moveRight scrollbarAwareOffsetF
                , Background.color colorMenuBackground
                , Border.rounded borderRadiusPanel
                ]
                (UI.Core.controlButton
                    { content = Icon Icons.Back
                    , onPress = RestoreGame
                    , selected = False
                    , disabled = False
                    , size = Large
                    }
                )

        Nothing ->
            Element.none
