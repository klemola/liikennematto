module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Config
    exposing
        ( borderRadius
        , borderSize
        , colors
        , whitespace
        )
import DebugPanel
import Editor
import Element
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Render
import Simulation exposing (Msg(..))
import Task
import UI exposing (ControlButtonSize(..))
import World exposing (World)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Simulation.subscriptions model.simulation
            |> Sub.map SimulationMsg
        , onResize ResizeWindow
        ]


type alias Model =
    { simulation : Simulation.Model
    , editor : Editor.Model
    , debugPanel : Maybe DebugPanel.Model
    , world : World
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( initialSimulationModel, simulationCmd ) =
            Simulation.init
    in
    ( { simulation = initialSimulationModel
      , editor = Editor.initialModel
      , debugPanel = Nothing
      , world = World.newWithInitialBoard
      }
    , Cmd.batch
        [ -- simulate a screen resize
          Task.perform (\{ viewport } -> ResizeWindow (round viewport.width) (round viewport.height)) getViewport
        , Cmd.map SimulationMsg simulationCmd
        ]
    )


type Msg
    = ResizeWindow Int Int
    | ToggleDebugMode
    | SimulationMsg Simulation.Msg
    | EditorMsg Editor.Msg
    | DebugPanelMsg DebugPanel.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            ( { model | world = World.withScreen ( width, height ) model.world }
            , Cmd.none
            )

        ToggleDebugMode ->
            let
                nextDebugPanel =
                    case model.debugPanel of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just DebugPanel.initialModel
            in
            ( { model | debugPanel = nextDebugPanel }
            , Cmd.none
            )

        SimulationMsg simulationMsg ->
            let
                ( simulation, nextWorld, cmd ) =
                    Simulation.update model.world simulationMsg model.simulation
            in
            ( { model
                | simulation = simulation
                , world = nextWorld
              }
            , Cmd.map SimulationMsg cmd
            )

        EditorMsg editorMsg ->
            let
                ( editor, nextWorld, cmd ) =
                    Editor.update model.world editorMsg model.editor
            in
            ( { model
                | editor = editor
                , world = nextWorld
              }
            , Cmd.map EditorMsg cmd
            )

        DebugPanelMsg debugPanelMsg ->
            let
                ( debugPanel, cmd ) =
                    model.debugPanel
                        |> Maybe.map (DebugPanel.update debugPanelMsg >> Tuple.mapFirst Just)
                        |> Maybe.withDefault ( model.debugPanel, Cmd.none )
            in
            ( { model
                | debugPanel = debugPanel
              }
            , Cmd.map DebugPanelMsg cmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Liikennematto"
    , body = [ ui model ]
    }


ui : Model -> Html Msg
ui model =
    let
        isRoadNetworkVisible =
            model.debugPanel
                |> Maybe.map DebugPanel.isRoadNetworkVisible
                |> Maybe.withDefault False

        render =
            Render.view model.world isRoadNetworkVisible
                |> Element.html

        editor =
            Editor.overlay model.world model.editor
                |> Element.map EditorMsg

        controls =
            Element.row
                [ Element.width Element.fill
                , Element.padding whitespace.tight
                , Background.color colors.menuBackground
                , Border.rounded borderRadius.heavy
                , Border.solid
                , Border.widthEach
                    { top = borderSize.light
                    , bottom = borderSize.light
                    , left = borderSize.heavy
                    , right = borderSize.heavy
                    }
                , Border.color colors.heavyBorder
                ]
                [ Editor.toolbar model.editor
                    |> Element.map EditorMsg
                , Element.row
                    [ Element.alignRight
                    , Element.spacing whitespace.tight
                    ]
                    [ UI.controlButton
                        { label = Element.text "🐛"
                        , onPress = ToggleDebugMode
                        , selected = model.debugPanel /= Nothing
                        , size = CBLarge
                        }
                    , UI.simulationControl
                        model.simulation
                        |> Element.map SimulationMsg
                    ]
                ]

        debugPanel =
            case model.debugPanel of
                Just debugPanelModel ->
                    DebugPanel.view debugPanelModel model.world
                        |> Element.map DebugPanelMsg

                Nothing ->
                    Element.none

        simulationBorderColor =
            case model.simulation.simulation of
                Simulation.Paused ->
                    colors.selected

                _ ->
                    colors.heavyBorder
    in
    Element.layout
        [ Background.color colors.mainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront debugPanel
        ]
        (Element.el
            [ Element.padding whitespace.regular
            , Element.centerX
            , Element.centerY
            ]
            (Element.column
                [ Element.spacing whitespace.regular
                ]
                [ Element.el
                    [ Element.inFront editor
                    , Element.alignTop
                    , Border.solid
                    , Border.width borderSize.heavy
                    , Border.rounded borderRadius.heavy
                    , Border.color simulationBorderColor
                    , Background.color colors.terrain
                    ]
                    render
                , controls
                , UI.projectInfo
                ]
            )
        )
