module UI exposing (layout, update)

import Data.Icons as Icons
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes as HtmlAttribute
import Message exposing (Message(..))
import Model.Editor exposing (mouseDetected)
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import Model.World exposing (World)
import UI.Core
    exposing
        ( borderRadiusButton
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


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    let
        ( modelWithDebugChanges, debugCmd ) =
            DebugPanel.update msg model

        ( modelWithEditorChanges, editorCmd ) =
            Editor.update msg modelWithDebugChanges
    in
    ( modelWithEditorChanges, Cmd.batch [ debugCmd, editorCmd ] )


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


layout : Liikennematto -> Element Message -> Element Message -> Html Message
layout model render renderDebugLayers =
    Element.layoutWith
        { options =
            if mouseDetected model.editor then
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
        , Element.inFront (restoreGameControl model.previousWorld)
        , Element.htmlAttribute (HtmlAttribute.id containerId)
        , Element.htmlAttribute (HtmlAttribute.style "touch-action" "pan-x pan-y")
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
                (Editor.overlay
                    model.renderCache
                    model.world
                    model.editor
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
        [ Editor.zoomControl model.editor
        , Element.el
            [ Element.padding whitespaceTight
            , Element.spacing whitespaceTight
            , Element.alignBottom
            , Background.color colorMenuBackground
            , Border.rounded borderRadiusPanel
            ]
            (simulationControl model.simulation)
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
                { iconKind = Icons.NewGame
                , onPress = NewGame
                , selected = False
                , disabled = False
                }
            , UI.Core.controlButton
                { iconKind = Icons.DebugPanel
                , onPress = ToggleDebugPanel
                , selected = model.debug.showDebugPanel
                , disabled = False
                }
            ]
        ]


simulationControl : SimulationState -> Element Message
simulationControl simulation =
    let
        ( iconKind, selected, msg ) =
            case simulation of
                Running ->
                    ( Icons.Pause, False, SetSimulation Paused )

                Paused ->
                    ( Icons.Resume, True, SetSimulation Running )
    in
    controlButton
        { iconKind = iconKind
        , onPress = msg
        , selected = selected
        , disabled = False
        }


debugPanel : Liikennematto -> Element Message
debugPanel model =
    if model.debug.showDebugPanel then
        DebugPanel.view model

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
                    { iconKind = Icons.Back
                    , onPress = RestoreGame
                    , selected = False
                    , disabled = False
                    }
                )

        Nothing ->
            Element.none
