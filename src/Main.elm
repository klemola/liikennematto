module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Game exposing (Msg(..))
import Html exposing (Html)
import Time


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ Time.every 1000 SlowTick
        , Time.every 500 FastTick
        ]


type alias Model =
    { game : Game.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { game = Game.initialModel }, Cmd.none )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix
    | GameMsg Game.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SlowTick _ ->
            let
                ( game, cmd ) =
                    Game.update UpdateEnvironment model.game
            in
            ( { model
                | game = game
              }
            , Cmd.map GameMsg cmd
            )

        FastTick _ ->
            let
                ( game, cmd ) =
                    Game.update UpdateTraffic model.game
            in
            ( { model
                | game =
                    game
              }
            , Cmd.map GameMsg cmd
            )

        GameMsg gameMsg ->
            let
                ( game, cmd ) =
                    Game.update gameMsg model.game
            in
            ( { model
                | game =
                    game
              }
            , Cmd.map GameMsg cmd
            )


view : Model -> Html Msg
view model =
    Game.view model.game |> svg
