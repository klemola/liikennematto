module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Simulation exposing (Msg(..))
import Task
import UI


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
        [ Simulation.subscriptions model.sharedState
            |> Sub.map SimulationMsg
        , onResize ResizeWindow
        ]


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
    = ResizeWindow Int Int
    | SimulationMsg Simulation.Msg
    | UIMsg UI.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


view : Model -> Browser.Document Msg
view model =
    { title = "Liikennematto"
    , body = [ Html.map UIMsg (UI.view model.sharedState model.ui) ]
    }
