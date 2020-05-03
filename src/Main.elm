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
    { game : Game.Model
    , ui : UI.Model
    , sharedState : SharedState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.initialModel
      , ui = UI.initialModel
      , sharedState = SharedState.initial
      }
    , Cmd.none
    )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix
    | GameMsg Game.Msg
    | UIMsg UI.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Room for improvement: Tick Msgs could be removed if Game subbed directly to ticks
        SlowTick _ ->
            let
                ( game, cmd, sharedStateUpdate ) =
                    Game.update model.sharedState UpdateEnvironment model.game

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | game = game, sharedState = nextSharedState }
            , Cmd.map GameMsg cmd
            )

        FastTick _ ->
            let
                ( game, cmd, sharedStateUpdate ) =
                    Game.update model.sharedState UpdateTraffic model.game

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | game = game, sharedState = nextSharedState }
            , Cmd.map GameMsg cmd
            )

        GameMsg gameMsg ->
            let
                ( game, cmd, sharedStateUpdate ) =
                    Game.update model.sharedState gameMsg model.game

                nextSharedState =
                    SharedState.update model.sharedState sharedStateUpdate
            in
            ( { model | game = game, sharedState = nextSharedState }
            , Cmd.map GameMsg cmd
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

        layout =
            Element.layout
                [ Background.color colors.mainBackground
                ]
                (Element.column
                    [ Element.centerX
                    , Element.centerY
                    , Element.padding paddingAmount
                    ]
                    [ Game.view model.sharedState
                        |> svg
                        |> Element.html
                    , UI.view model.sharedState model.ui
                    ]
                )
    in
    Html.map UIMsg layout
