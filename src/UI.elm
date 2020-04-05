module UI exposing (..)

import Car exposing (Status(..), TurnKind(..))
import Config
import Coords
import Game
import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (src, style, width)


view : Game.Model -> Html msg
view game =
    let
        padding =
            16

        uiWidth =
            String.fromInt (Config.boardSizePx - 2 * padding) ++ "px"

        showCarKind car =
            img [ src ("assets/" ++ Car.asset car), width 16 ] []

        carStatusToString status =
            case status of
                Moving ->
                    "Moving"

                Turning LeftTurn ->
                    "Turning left"

                Turning RightTurn ->
                    "Turning right"

                Turning UTurn ->
                    "Making a U-turn"

                Waiting ->
                    "Waiting"

                StoppedAtIntersection timeRemaining ->
                    "Stopped, time remaining: " ++ String.fromInt timeRemaining

                Yielding ->
                    "Yielding"

        carInfo car =
            div [ style "display" "flex", style "align-items" "center", style "margin-bottom" "2px" ]
                [ showCarKind car
                , span [ style "margin-left" "1rem" ] [ text (String.join " | " [ Coords.toString car.coords, carStatusToString car.status ]) ]
                ]
    in
    -- Room for improvement: should try to use elm-ui instead of ad hoc HTML
    div
        [ style "width" uiWidth
        , style "font-family" "monospace"
        , style "margin-top" "1rem"
        , style "background-color" "#bfd5d9"
        , style "color" "#344143"
        , style "padding" (String.fromInt padding ++ "px")
        , style "border-radius" "0.5rem"
        ]
        (List.map carInfo game.cars)
