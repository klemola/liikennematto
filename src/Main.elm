module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Config
    exposing
        ( boardSizeScaled
        , borderRadius
        , borderSize
        , colors
        , uiDimensions
        , whitespace
        )
import DebugPanel
import Editor
import Element exposing (Element)
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
    , screen : Screen
    }


type alias Screen =
    { width : Int
    , height : Int
    , orientation : Element.Orientation
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
      , world = World.default
      , screen =
            { width = 375
            , height = 667
            , orientation = Element.Landscape
            }
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
            ( { model
                | screen =
                    { width = width
                    , height = height
                    , orientation =
                        if width < height then
                            Element.Portrait

                        else
                            Element.Landscape
                    }
              }
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
                ( nextSimulation, nextWorld, cmd ) =
                    Simulation.update model.world simulationMsg model.simulation
            in
            ( { model
                | simulation = nextSimulation
                , world = nextWorld
              }
            , Cmd.map SimulationMsg cmd
            )

        EditorMsg editorMsg ->
            let
                ( nextEditor, nextWorld, cmd ) =
                    Editor.update model.world editorMsg model.editor
            in
            ( { model
                | editor = nextEditor
                , world = nextWorld
              }
            , Cmd.map EditorMsg cmd
            )

        DebugPanelMsg debugPanelMsg ->
            let
                ( nextDebugPanel, cmd ) =
                    model.debugPanel
                        |> Maybe.map (DebugPanel.update debugPanelMsg >> Tuple.mapFirst Just)
                        |> Maybe.withDefault ( model.debugPanel, Cmd.none )
            in
            ( { model
                | debugPanel = nextDebugPanel
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
    Element.layout
        [ Background.color colors.mainBackground
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.inFront (controls model)
        , Element.inFront UI.projectInfo
        , Element.inFront (debugPanel model)
        ]
        (render model)


render : Model -> Element Msg
render model =
    let
        debugLayers =
            { showRoadNetwork =
                model.debugPanel
                    |> Maybe.map .showRoadNetwork
                    |> Maybe.withDefault False
            , showCarDebugVisuals =
                model.debugPanel
                    |> Maybe.map .showCarDebugVisuals
                    |> Maybe.withDefault False
            }

        renderedSize =
            floor boardSizeScaled

        ( viewportWidth, viewportHeight ) =
            ( min model.screen.width renderedSize, min model.screen.height renderedSize )

        ( overflowStrategy, showBorders ) =
            if renderedSize > model.screen.width || renderedSize > model.screen.height then
                ( Element.scrollbars, False )

            else
                ( Element.clip, True )

        maybeBorder =
            if showBorders then
                [ Border.solid
                , Border.width borderSize.heavy
                , Border.rounded borderRadius.heavy
                , Border.color
                    (case model.simulation.simulation of
                        Simulation.Paused ->
                            colors.selected

                        _ ->
                            colors.heavyBorder
                    )
                ]

            else
                []
    in
    Render.view model.world debugLayers
        |> Element.html
        |> Element.el
            ([ Element.width (Element.px viewportWidth)
             , Element.height (Element.px viewportHeight)
             , Element.inFront
                (Editor.overlay model.world model.editor
                    |> Element.map EditorMsg
                )
             , Element.centerX
             , Element.centerY
             , Background.color colors.terrain
             , overflowStrategy
             ]
                |> List.append maybeBorder
            )


controls : Model -> Element Msg
controls model =
    let
        width =
            min model.screen.width (floor boardSizeScaled)

        controlButtonSize =
            if min model.screen.width model.screen.height < uiDimensions.smallControlsBreakpoint then
                CBSmall

            else
                CBLarge
    in
    Element.row
        [ Element.width (Element.px width)
        , Element.padding whitespace.tight
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
            |> Element.map EditorMsg
        , Element.row
            [ Element.alignRight
            , Element.spacing whitespace.tight
            ]
            [ UI.controlButton
                { label = Element.text "🐛"
                , onPress = ToggleDebugMode
                , selected = model.debugPanel /= Nothing
                , disabled = False
                , size = controlButtonSize
                }
            , UI.carSpawnControl model.simulation controlButtonSize
                |> Element.map SimulationMsg
            , UI.simulationControl model.simulation controlButtonSize
                |> Element.map SimulationMsg
            ]
        ]


debugPanel : Model -> Element Msg
debugPanel model =
    case model.debugPanel of
        Just debugPanelModel ->
            DebugPanel.view debugPanelModel model.world
                |> Element.map DebugPanelMsg

        Nothing ->
            Element.none
