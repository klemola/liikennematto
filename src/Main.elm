module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Collage.Render as Render
import Element
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import SharedState exposing (SharedState, SharedStateUpdate(..), SimulationState(..))
import Simulation exposing (Msg(..))
import Task
import Time
import UI


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }


subs : Model -> Sub Msg
subs model =
    case model.sharedState.simulationState of
        Simulation speed ->
            let
                ( slowTickSpeed, fastTickSpeed ) =
                    SharedState.simulationSpeedValues speed
            in
            Sub.batch
                [ Time.every slowTickSpeed SlowTick
                , Time.every fastTickSpeed FastTick
                , onResize ResizeWindow
                ]

        Paused ->
            onResize ResizeWindow


type alias Model =
    { simulation : Simulation.Model
    , ui : UI.Model
    , sharedState : SharedState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { simulation = Simulation.initialModel
      , ui = UI.initialModel
      , sharedState = SharedState.initial
      }
      -- simulate a screen resize
    , Task.perform (\{ viewport } -> ResizeWindow (round viewport.width) (round viewport.height)) getViewport
    )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix
    | ResizeWindow Int Int
    | SimulationMsg Simulation.Msg
    | UIMsg UI.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Room for improvement: Tick Msgs could be removed if Simulation subbed directly to ticks
        SlowTick _ ->
            let
                ( simulation, cmd, sharedStateUpdate ) =
                    Simulation.update model.sharedState UpdateEnvironment model.simulation

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | simulation = simulation, sharedState = nextSharedState }
            , Cmd.map SimulationMsg cmd
            )

        FastTick _ ->
            let
                ( simulation, cmd, sharedStateUpdate ) =
                    Simulation.update model.sharedState UpdateTraffic model.simulation

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | simulation = simulation, sharedState = nextSharedState }
            , Cmd.map SimulationMsg cmd
            )

        ResizeWindow width height ->
            let
                nextSharedState =
                    SharedState.update model.sharedState (RecalculateDimensions width height)
            in
            ( { model | sharedState = nextSharedState }, Cmd.none )

        SimulationMsg simulationMsg ->
            let
                ( simulation, cmd, sharedStateUpdate ) =
                    Simulation.update model.sharedState simulationMsg model.simulation

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | simulation = simulation, sharedState = nextSharedState }
            , Cmd.map SimulationMsg cmd
            )

        UIMsg uiMsg ->
            let
                ( ui, cmd, sharedStateUpdate ) =
                    UI.update model.sharedState uiMsg model.ui

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | ui = ui, sharedState = nextSharedState }
            , Cmd.map UIMsg cmd
            )


view : Model -> Html Msg
view model =
    let
        simulation =
            Simulation.view model.sharedState
                |> Render.svg
                |> Element.html

        editor =
            UI.editor model.sharedState model.ui

        toolbar =
            UI.toolbar model.ui model.sharedState.dimensions

        menu =
            UI.menu model.sharedState

        simulationBorderColor =
            case model.sharedState.simulationState of
                Paused ->
                    UI.colors.selected

                _ ->
                    UI.colors.heavyBorder

        layout =
            Element.layout
                [ Background.color UI.colors.mainBackground
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                (Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding UI.whitespace.regular
                    ]
                    (Element.row
                        [ Element.spacing UI.whitespace.regular
                        ]
                        [ toolbar
                        , Element.el
                            [ Element.inFront editor
                            , Element.alignTop
                            , Border.solid
                            , Border.width UI.borderSize.heavy
                            , Border.rounded UI.borderRadius.heavy
                            , Border.color simulationBorderColor
                            ]
                            simulation
                        , menu
                        ]
                    )
                )
    in
    Html.map UIMsg layout
