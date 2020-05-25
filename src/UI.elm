module UI exposing (Model, Msg(..), initialModel, menu, update, view)

import Car exposing (Car)
import Config exposing (borderRadius, borderSize, colors, whitespace)
import Coords
import Dict
import Direction exposing (Orientation(..))
import Editor
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , image
        , inFront
        , padding
        , paddingXY
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphics
import Html exposing (Html)
import Render
import SharedState
    exposing
        ( Dimensions
        , SharedState
        , SharedStateUpdate
        , SimulationSpeed(..)
        , SimulationState(..)
        )


type Msg
    = SetSimulationState SimulationState
    | EditorMsg Editor.Msg


type alias Model =
    { editor : Editor.Model
    }


initialModel : Model
initialModel =
    { editor = Editor.initialModel }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SetSimulationState state ->
            ( model, Cmd.none, SharedState.UpdateSimulationState state )

        EditorMsg editorMsg ->
            let
                ( editor, cmd, sharedStateUpdate ) =
                    Editor.update sharedState editorMsg model.editor
            in
            ( { model | editor = editor }
            , Cmd.map EditorMsg cmd
            , sharedStateUpdate
            )


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        simulation =
            Render.view sharedState
                |> Element.html

        editor =
            Editor.overlay sharedState model.editor
                |> Element.map EditorMsg

        toolbar =
            Editor.toolbar model.editor sharedState.dimensions
                |> Element.map EditorMsg

        simulationBorderColor =
            case sharedState.simulationState of
                Paused ->
                    colors.selected

                _ ->
                    colors.heavyBorder
    in
    Element.layout
        [ Background.color colors.mainBackground
        , width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , padding whitespace.regular
            ]
            (row
                [ spacing whitespace.regular
                ]
                [ toolbar
                , el
                    [ inFront editor
                    , alignTop
                    , Border.solid
                    , Border.width borderSize.heavy
                    , Border.rounded borderRadius.heavy
                    , Border.color simulationBorderColor
                    , Background.color colors.terrain
                    ]
                    simulation
                , menu sharedState
                ]
            )
        )


menu : SharedState -> Element Msg
menu sharedState =
    column
        [ Font.family [ Font.monospace ]
        , Font.color colors.text
        , Font.size sharedState.dimensions.text
        , alignTop
        , width (px sharedState.dimensions.menu)
        , paddingXY whitespace.tight whitespace.regular
        , spacing whitespace.regular
        , Background.color colors.toolbarBackground
        , Border.rounded borderRadius.heavy
        , Border.solid
        , Border.widthEach
            { top = borderSize.heavy
            , bottom = borderSize.heavy
            , left = borderSize.light
            , right = borderSize.light
            }
        , Border.color colors.heavyBorder
        ]
        [ simulationControl sharedState
        , debug sharedState
        ]


simulationControl : SharedState -> Element Msg
simulationControl { simulationState, dimensions } =
    let
        isSelected speed =
            case simulationState of
                Simulation currentSpeed ->
                    speed == currentSpeed

                Paused ->
                    False
    in
    row
        [ width fill
        , spacing whitespace.tight
        ]
        [ controlButton dimensions "⏸️" (SetSimulationState Paused) (simulationState == Paused)
        , controlButton dimensions "🐌" (SetSimulationState (Simulation Slow)) (isSelected Slow)
        , controlButton dimensions "🐇" (SetSimulationState (Simulation Medium)) (isSelected Medium)
        , controlButton dimensions "🐆" (SetSimulationState (Simulation Fast)) (isSelected Fast)
        ]


controlButton : Dimensions -> String -> Msg -> Bool -> Element Msg
controlButton dimensions label msg selected =
    Input.button
        [ Background.color colors.buttonBackground
        , width (fillPortion 1)
        , Font.size dimensions.menuButton
        , padding whitespace.tight
        , Border.width borderSize.light
        , Border.rounded borderRadius.light
        , Border.solid
        , Border.color
            (if selected then
                colors.selected

             else
                colors.lightBorder
            )
        ]
        { onPress = Just msg
        , label = text label
        }


debug : SharedState -> Element Msg
debug sharedState =
    column [ spacing whitespace.tight, width fill ]
        (Dict.values sharedState.cars
            |> List.map (carInfo sharedState.dimensions)
        )


carInfo : Dimensions -> Car -> Element msg
carInfo dimensions car =
    let
        showCarKind =
            image [ width (px dimensions.text) ]
                { description = ""
                , src = "assets/" ++ Graphics.carAsset car
                }
    in
    row
        [ width fill
        , padding whitespace.tight
        , spacing whitespace.regular
        , Font.color colors.textInverse
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ showCarKind
        , column [ spacing whitespace.tight ]
            [ text (Coords.toString car.coords)
            , text (Car.statusDescription car.status)
            ]
        ]
