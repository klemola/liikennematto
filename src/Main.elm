module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Config
import Element
import Element.Background as Background
import Html exposing (Html)
import SharedState exposing (SharedState, SimulationState(..))
import Simulation exposing (Msg(..))
import Time
import UI exposing (Msg(..), colors)


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
    let
        ( slowTickSpeed, fastTickSpeed ) =
            SharedState.simulationSpeedValues model.sharedState.simulationSpeed
    in
    case model.sharedState.simulationState of
        Simulation ->
            Sub.batch
                [ Time.every slowTickSpeed SlowTick
                , Time.every fastTickSpeed FastTick
                ]

        Paused ->
            Sub.none


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
    , Cmd.none
    )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix
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
        paddingAmount =
            floor (Config.tileSize / 2)

        simulation =
            Simulation.view model.sharedState
                |> svg
                |> Element.html

        ui =
            UI.view model.sharedState model.ui

        layout =
            Element.layout
                [ Background.color colors.mainBackground
                , Element.padding paddingAmount
                ]
                (Element.el
                    [ Element.centerX
                    , Element.inFront ui
                    ]
                    simulation
                )
    in
    Html.map UIMsg layout
