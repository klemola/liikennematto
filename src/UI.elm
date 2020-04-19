module UI exposing (..)

import Car exposing (Status(..), TurnKind(..))
import Config
import Coords
import Dict
import Game
import Html exposing (Html, button, div, h4, img, span, text)
import Html.Attributes exposing (src, style, width)
import Html.Events exposing (onClick)
import SharedState exposing (Mode(..), SharedState, SimulationSpeed(..))


type Msg
    = ToggleSimulation
    | SetSimulationSpeed SimulationSpeed


view : Game.Model -> SharedState -> Html Msg
view game sharedState =
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

                StoppedAtIntersection roundsRemaining ->
                    "Stopped, rounds remaining: " ++ String.fromInt roundsRemaining

                Yielding ->
                    "Yielding"

        carInfo car =
            div
                [ style "display" "flex"
                , style "align-items" "center"
                , style "margin-bottom" "2px"
                , style "white-space" "pre"
                ]
                [ showCarKind car
                , span
                    [ style "margin-left" "1rem" ]
                    [ text (String.join " | " [ Coords.toString car.coords, carStatusToString car.status ]) ]
                ]

        modeAsText =
            case sharedState.mode of
                Simulation ->
                    "Pause"

                Paused ->
                    "Resume"

        buttonStyle simulationSpeed =
            if simulationSpeed == sharedState.simulationSpeed then
                style "border" "1px solid black"

            else
                style "border" "1px solid white"
    in
    -- Room for improvement: should try to use elm-ui instead of ad hoc HTML
    div
        [ style "display" "flex"
        , style "width" uiWidth
        , style "font-family" "monospace"
        , style "margin-top" "1rem"
        , style "background-color" "#bfd5d9"
        , style "color" "#344143"
        , style "padding" (String.fromInt padding ++ "px")
        , style "border-radius" "0.5rem"
        ]
        [ div [ style "flex" "1" ]
            (Dict.values game.cars
                |> List.map carInfo
            )
        , div []
            [ h4 [ style "margin-top" "0" ] [ text "Simulation control" ]
            , button [ onClick ToggleSimulation ] [ text modeAsText ]
            , h4 [] [ text "Simulation speed" ]
            , button [ buttonStyle Slow, onClick (SetSimulationSpeed Slow) ] [ text "Slow" ]
            , button [ buttonStyle Medium, onClick (SetSimulationSpeed Medium) ] [ text "Medium" ]
            , button [ buttonStyle Fast, onClick (SetSimulationSpeed Fast) ] [ text "Fast" ]
            ]
        ]
