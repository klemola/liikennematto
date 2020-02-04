module Main exposing (main)

import Board exposing (Board)
import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Collage.Render exposing (svg)
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs _ =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)


type alias Delta =
    Float


type alias Model =
    { board : Board
    , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.init, time = 0 }, Cmd.none )


type Msg
    = Tick Delta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                newTime =
                    model.time + dt

                secsPassed =
                    round model.time

                newSecs =
                    round newTime

                -- Temporarily limit ticks to ~one per sec
                shouldUpdate =
                    secsPassed /= newSecs
            in
            ( { model
                | board =
                    if shouldUpdate then
                        tick dt model.board

                    else
                        model.board
                , time = newTime
              }
            , Cmd.none
            )


tick : Float -> Board -> Board
tick dt board =
    Board.update board


view : Model -> Html Msg
view model =
    Board.view model.board |> svg
