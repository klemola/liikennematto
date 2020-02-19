module Main exposing (main)

import Board exposing (Board, Msg(..))
import Browser
import Collage.Render exposing (svg)
import Html exposing (Html)
import Time


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ Time.every 1000 SlowTick
        , Time.every 600 FastTick
        ]


type alias Model =
    { board : Board
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init }, Cmd.none )


type Msg
    = SlowTick Time.Posix
    | FastTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SlowTick _ ->
            ( { model
                | board = Board.update UpdateEnvironment model.board
              }
            , Cmd.none
            )

        FastTick _ ->
            ( { model
                | board =
                    Board.update UpdateTraffic model.board
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Board.view model.board |> svg
