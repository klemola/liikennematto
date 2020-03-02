module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Direction exposing (Direction(..))
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
        , Time.every 400 FastTick
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SlowTick _ ->
            ( { model
                | game = Game.update UpdateEnvironment model.game
              }
            , Cmd.none
            )

        FastTick _ ->
            ( { model
                | game =
                    Game.update UpdateTraffic model.game
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Game.view model.game |> svg
