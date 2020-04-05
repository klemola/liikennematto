module Main exposing (main)

import Browser
import Collage.Render exposing (svg)
import Config
import Game exposing (Msg(..))
import Graphics
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Time
import UI


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
    let
        padding =
            String.fromFloat Config.tileSize ++ "px"

        minHeight =
            "calc(100vh - " ++ String.fromFloat (2 * Config.tileSize) ++ "px)"
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
        , UI.view model.game
        ]
