module UI.UI exposing (layout, update)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import UI.Core
    exposing
        ( borderRadius
        , colors
        , containerId
        , controlButton
        , scrollbarAwareOffsetF
        , whitespace
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


layout : Liikennematto -> (Liikennematto -> Element Message) -> Html Message
layout model renderFn =
    Element.layout
        [ Background.color colors.mainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        , Element.inFront (controls model)
        , Element.inFront (Editor.zoomControl model.editor)
        , Element.inFront (debugPanel model)
        , Element.htmlAttribute (Html.Attributes.id containerId)
        , Element.htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        ]
        (renderFn model)


controls : Liikennematto -> Element Message
controls model =
    Element.row
        [ Element.padding whitespace.tight
        , Element.spacing whitespace.tight
        , Element.alignBottom
        , Element.centerX
        , Element.moveUp scrollbarAwareOffsetF
        , Background.color colors.menuBackground
        , Border.rounded borderRadius.light
        ]
        [ Editor.toolbar model.editor
        , Element.row
            [ Element.spacing whitespace.tight
            ]
            [ UI.Core.controlButton
                { label = Element.text "🐛"
                , onPress = ToggleDebugMode
                , selected = model.showDebugPanel
                , disabled = False
                }
            , simulationControl model.simulation
            ]
        ]


simulationControl : SimulationState -> Element Message
simulationControl simulation =
    let
        ( label, selected, msg ) =
            case simulation of
                Running ->
                    ( "⏸️", False, SetSimulation Paused )

                Paused ->
                    ( "▶️", True, SetSimulation Running )
    in
    controlButton
        { label = Element.text label
        , onPress = msg
        , selected = selected
        , disabled = False
        }


debugPanel : Liikennematto -> Element Message
debugPanel model =
    if model.showDebugPanel then
        DebugPanel.view model

    else
        Element.none
