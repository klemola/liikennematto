module UI.UI exposing (layout, update)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Message exposing (Message(..))
import Model.Liikennematto exposing (Liikennematto, SimulationState(..))
import UI.Core exposing (ControlButtonSize(..), borderRadius, colors, controlButton, link, uiDimensions, whitespace)
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
        , Element.inFront projectInfo
        , Element.inFront (debugPanel model)
        ]
        (renderFn model)


controls : Liikennematto -> Element Message
controls model =
    let
        controlButtonSize =
            if min model.screen.width model.screen.height < uiDimensions.smallControlsBreakpoint then
                CBSmall

            else
                CBLarge

        spacer =
            Element.el
                [ Element.width (Element.px whitespace.tight)
                , Element.height (Element.px uiDimensions.controlButtonS)
                , Background.color colors.mainBackground
                , Border.rounded borderRadius.heavy
                ]
                Element.none
    in
    Element.row
        [ Element.padding whitespace.tight
        , Element.spacing whitespace.regular
        , Element.alignBottom
        , Element.centerX
        , Background.color colors.menuBackground
        , Border.roundEach
            { topLeft = borderRadius.heavy
            , topRight = borderRadius.heavy
            , bottomLeft = 0
            , bottomRight = 0
            }
        , Border.solid
        , Border.color colors.heavyBorder
        ]
        [ Editor.toolbar model.editor controlButtonSize
        , spacer
        , Element.row
            [ Element.spacing whitespace.tight
            ]
            [ UI.Core.controlButton
                { label = Element.text "🐛"
                , onPress = ToggleDebugMode
                , selected = model.showDebugPanel
                , disabled = False
                , size = controlButtonSize
                }
            , Editor.carSpawnControl model.editor controlButtonSize
            , simulationControl model.simulation controlButtonSize
            ]
        ]


simulationControl : SimulationState -> ControlButtonSize -> Element Message
simulationControl simulation controlButtonSize =
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
        , size = controlButtonSize
        }


debugPanel : Liikennematto -> Element Message
debugPanel model =
    if model.showDebugPanel then
        DebugPanel.view model

    else
        Element.none


projectInfo : Element msg
projectInfo =
    Element.row
        [ Element.width Element.shrink
        , Element.padding whitespace.regular
        , Element.spacing whitespace.regular
        , Element.alignRight
        , Font.family
            [ Font.typeface "Helvetica"
            , Font.typeface "sans-serif"
            ]
        , Font.size uiDimensions.text
        , Background.color colors.textInverse
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomRight = 0
            , bottomLeft = borderRadius.heavy
            }
        ]
        [ Element.el [ Font.bold ] (Element.text "Liikennematto")
        , Element.row
            [ Element.spacing whitespace.tight
            , Element.alignRight
            ]
            [ link "https://github.com/klemola/liikennematto" "GitHub"
            , Element.text "｜"
            , link "https://matiasklemola.com/liikennematto-dev-blog-one" "Blog"
            , Element.text "｜"
            , link "https://twitter.com/MatiasKlemola" "Twitter"
            ]
        ]
