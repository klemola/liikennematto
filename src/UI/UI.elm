module UI.UI exposing (layout, update)

import Data.Icons as Icons
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
        , icon
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
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow =
                    Nothing
                }
            ]
        }
        [ Background.color colors.mainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.scrollbars
        , Element.inFront (rightControls model)
        , Element.inFront (leftControls model)
        , Element.inFront (debugPanel model)
        , Element.htmlAttribute (Html.Attributes.id containerId)
        , Element.htmlAttribute (Html.Attributes.style "touch-action" "pan-x pan-y")
        ]
        (renderFn model)


leftControls : Liikennematto -> Element Message
leftControls model =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignBottom
        , Element.alignLeft
        , Element.moveUp scrollbarAwareOffsetF
        , Element.moveRight scrollbarAwareOffsetF
        ]
        [ Editor.zoomControl model.editor
        , Element.el
            [ Element.padding whitespace.tight
            , Element.spacing whitespace.tight
            , Element.alignBottom
            , Background.color colors.menuBackground
            , Border.rounded borderRadius
            ]
            (simulationControl model.simulation)
        ]


rightControls : Liikennematto -> Element Message
rightControls model =
    Element.row
        [ Element.padding whitespace.tight
        , Element.spacing whitespace.tight
        , Element.alignBottom
        , Element.alignRight
        , Element.moveUp scrollbarAwareOffsetF
        , Element.moveLeft scrollbarAwareOffsetF
        , Background.color colors.menuBackground
        , Border.rounded borderRadius
        ]
        [ Element.row
            [ Element.spacing whitespace.tight
            ]
            [ UI.Core.controlButton
                { label = icon Icons.NewGame
                , onPress = ResetWorld
                , selected = False
                , disabled = False
                }
            , UI.Core.controlButton
                { label = icon Icons.ToggleDebug
                , onPress = ToggleDebugMode
                , selected = model.showDebugPanel
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
        { label = icon iconKind
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
