module Main exposing (main)

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onResize)
import Html
import Simulation exposing (Msg(..))
import Task
import UI
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
        [ Simulation.subscriptions model.world
            |> Sub.map SimulationMsg
        , onResize ResizeWindow
        ]


type alias Model =
    { simulation : Simulation.Model
    , ui : UI.Model
    , world : World
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( initialSimulationModel, simulationCmd ) =
            Simulation.init
    in
    ( { simulation = initialSimulationModel
      , ui = UI.initialModel
      , world = World.new
      }
    , Cmd.batch
        [ -- simulate a screen resize
          Task.perform (\{ viewport } -> ResizeWindow (round viewport.width) (round viewport.height)) getViewport
        , Cmd.map SimulationMsg simulationCmd
        ]
    )


type Msg
    = ResizeWindow Int Int
    | SimulationMsg Simulation.Msg
    | UIMsg UI.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            ( { model | world = World.setScreen ( width, height ) model.world }
            , UI.recalculateDimensions ( width, height )
                |> Cmd.map UIMsg
            )

        SimulationMsg simulationMsg ->
            let
                ( simulation, nextWorld, cmd ) =
                    Simulation.update model.world simulationMsg model.simulation
            in
            ( { model | simulation = simulation, world = nextWorld }
            , Cmd.map SimulationMsg cmd
            )

        UIMsg uiMsg ->
            let
                ( ui, nextWorld, cmd ) =
                    UI.update model.world uiMsg model.ui
            in
            ( { model | ui = ui, world = nextWorld }
            , Cmd.map UIMsg cmd
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Liikennematto"
    , body = [ Html.map UIMsg (UI.view model.world model.ui) ]
    }
