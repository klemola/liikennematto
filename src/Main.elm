module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Config
import Game exposing (Msg(..))
import Graphics
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import SharedState exposing (Mode(..), SharedState)
import Time
import UI exposing (Msg(..))


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs model =
    let
        ( slowTickSpeed, fastTickSpeed ) =
            SharedState.simulationSpeedValues model.sharedState.simulationSpeed
    in
    case model.sharedState.mode of
        Simulation ->
            Sub.batch
                [ Time.every slowTickSpeed SlowTick
                , Time.every fastTickSpeed FastTick
                ]

        Paused ->
            Sub.none


type alias Model =
    { game : Game.Model
    , sharedState : SharedState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.initialModel, sharedState = SharedState.initial }, Cmd.none )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix
    | GameMsg Game.Msg
    | UIMsg UI.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SlowTick _ ->
            let
                ( game, cmd ) =
                    Game.update UpdateEnvironment model.game
            in
            ( { model | game = game }
            , Cmd.map GameMsg cmd
            )

        FastTick _ ->
            let
                ( game, cmd ) =
                    Game.update UpdateTraffic model.game
            in
            ( { model | game = game }
            , Cmd.map GameMsg cmd
            )

        GameMsg gameMsg ->
            let
                ( game, cmd ) =
                    Game.update gameMsg model.game
            in
            ( { model | game = game }
            , Cmd.map GameMsg cmd
            )

        UIMsg uiMsg ->
            let
                ss =
                    model.sharedState

                updatedSharedState =
                    case uiMsg of
                        ToggleSimulation ->
                            { ss | mode = SharedState.nextMode ss.mode }

                        SetSimulationSpeed speed ->
                            { ss | simulationSpeed = speed }
            in
            ( { model | sharedState = updatedSharedState }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        padding =
            String.fromFloat (Config.tileSize / 2) ++ "px"

        minHeight =
            "calc(100vh - " ++ String.fromFloat Config.tileSize ++ "px)"
    in
    div
        [ style "display" "flex"
        , style "background-color" Graphics.backgroundCss
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "padding" padding
        , style "min-height" minHeight
        ]
        [ Game.view model.game |> svg
        , Html.map UIMsg (UI.view model.game model.sharedState)
        ]
