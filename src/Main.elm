module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events as Events
import Config exposing (boardSizeScaled)
import Defaults
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Model.World exposing (World)
import Pixels
import Render
import Simulation.Simulation as Simulation
import Task
import UI.Core
    exposing
        ( ControlButtonSize(..)
        , borderRadius
        , colors
        , uiDimensions
        , whitespace
        )
import UI.DebugPanel
import UI.Editor
import UI.UI


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
        , Events.onResize ResizeWindow
        ]


type alias Model =
    { simulation : Simulation.Model
    , editor : UI.Editor.Model
    , debugPanel : Maybe UI.DebugPanel.Model
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
      , editor = UI.Editor.initialModel
      , debugPanel = Nothing
      , world = Defaults.defaultWorld
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
    | EditorMsg UI.Editor.Msg
    | DebugPanelMsg UI.DebugPanel.Msg


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
                            Just UI.DebugPanel.initialModel
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
                    UI.Editor.update model.world editorMsg model.editor
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
                        |> Maybe.map (UI.DebugPanel.update debugPanelMsg >> Tuple.mapFirst Just)
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
        , Element.inFront UI.UI.projectInfo
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
            boardSizeScaled |> Pixels.inPixels

        ( viewportWidth, viewportHeight ) =
            ( min model.screen.width renderedSize, min model.screen.height renderedSize )

        overflowStrategy =
            if renderedSize > model.screen.width || renderedSize > model.screen.height then
                Element.scrollbars

            else
                Element.clip
    in
    Render.view model.world debugLayers
        |> Element.html
        -- render + overlay
        |> Element.el
            [ Element.width (Element.px renderedSize)
            , Element.height (Element.px renderedSize)
            , Element.inFront
                (UI.Editor.overlay model.world model.editor
                    |> Element.map EditorMsg
                )
            , Background.color colors.terrain
            ]
        -- overflow wrapper
        |> Element.el
            [ Element.width (Element.px viewportWidth)
            , Element.height (Element.px viewportHeight)
            , Element.centerX
            , Element.centerY
            , Border.rounded borderRadius.heavy
            , overflowStrategy
            ]


controls : Model -> Element Msg
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
        [ UI.Editor.toolbar model.editor controlButtonSize
            |> Element.map EditorMsg
        , spacer
        , Element.row
            [ Element.spacing whitespace.tight
            ]
            [ UI.Core.controlButton
                { label = Element.text "🐛"
                , onPress = ToggleDebugMode
                , selected = model.debugPanel /= Nothing
                , disabled = False
                , size = controlButtonSize
                }
            , UI.UI.carSpawnControl model.simulation controlButtonSize
                |> Element.map SimulationMsg
            , UI.UI.simulationControl model.simulation controlButtonSize
                |> Element.map SimulationMsg
            ]
        ]


debugPanel : Model -> Element Msg
debugPanel model =
    case model.debugPanel of
        Just debugPanelModel ->
            UI.DebugPanel.view debugPanelModel model.world
                |> Element.map DebugPanelMsg

        Nothing ->
            Element.none
