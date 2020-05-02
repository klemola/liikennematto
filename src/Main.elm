module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Config
import Element
import Element.Background as Background
import Game exposing (Msg(..))
import Html exposing (Html)
import SharedState exposing (SharedState, SimulationState(..))
import Time
import UI exposing (Msg(..), colors)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


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
                            { ss | simulationState = SharedState.nextSimulationState ss.simulationState }

                        SetSimulationSpeed speed ->
                            { ss | simulationSpeed = speed }
            in
            ( { model | sharedState = updatedSharedState }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        paddingAmount =
            floor (Config.tileSize / 2)

        layout =
            Element.layout
                [ Background.color colors.mainBackground
                ]
                (Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding paddingAmount
                    ]
                    [ Game.view model.game
                        |> svg
                        |> Element.html
                    , UI.debug model.game model.sharedState
                    ]
                )
    in
    Html.map UIMsg layout
