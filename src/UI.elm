module UI exposing (..)

import Board
import Car exposing (Car, Status(..), TurnKind(..))
import Config
import Coords exposing (Coords)
import Dict
import Direction exposing (Orientation(..))
import Element exposing (Element, alignRight, alignTop, centerY, column, el, fill, height, image, mouseOver, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate, SimulationSpeed(..), SimulationState(..))
import Tile exposing (RoadKind(..), Tile)


type Tool
    = Construction Tile
    | Bulldozer
    | None


type alias Model =
    { selectedTool : Tool
    }


type Msg
    = ToggleSimulation
    | SetSimulationSpeed SimulationSpeed
    | SelectTile Coords


initialModel : Model
initialModel =
    { selectedTool = Construction (Tile.TwoLaneRoad (Regular Vertical))
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        ToggleSimulation ->
            let
                simulationState =
                    SharedState.nextSimulationState sharedState.simulationState
            in
            ( model, Cmd.none, SharedState.UpdateSimulationState simulationState )

        SetSimulationSpeed speed ->
            ( model, Cmd.none, SharedState.UpdateSimulationSpeed speed )

        SelectTile coords ->
            case ( model.selectedTool, Board.get coords sharedState.board ) of
                ( Construction tile, Nothing ) ->
                    ( model
                    , Cmd.none
                    , Board.set coords tile sharedState.board
                        |> SharedState.UpdateBoard
                    )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )


colors =
    { mainBackground = Element.rgb255 33 191 154
    , toolbarBackground = rgb255 191 213 217
    , buttonBackground = rgb255 255 255 255
    , text = rgb255 52 65 67
    , selected = rgb255 0 0 0
    , grid = rgb255 222 222 222
    }


view : SharedState -> Model -> Element Msg
view sharedState model =
    column [ width fill, spacing 10 ]
        [ editor sharedState model
        , debug sharedState
        ]


editor : SharedState -> Model -> Element Msg
editor sharedState model =
    let
        rg =
            List.range 1 Config.boardSize

        col y =
            List.map (\x -> tileOverlay ( x, y )) rg

        rows =
            List.map (\y -> row [] (col y)) rg
    in
    column [] rows


tileOverlay : Coords -> Element Msg
tileOverlay coords =
    let
        tileSizePx =
            px (floor Config.tileSize)
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow colors.grid 5 ]
        , Events.onClick (SelectTile coords)
        ]
        Element.none


debug : SharedState -> Element Msg
debug sharedState =
    let
        simulationStateAsText =
            case sharedState.simulationState of
                Simulation ->
                    "Pause"

                Paused ->
                    "Resume"

        controlLabel t =
            el [ Font.semiBold ] (text t)

        controlButton t m s =
            Input.button
                [ Background.color colors.buttonBackground
                , padding 5
                , Border.width 2
                , Border.solid
                , Border.color
                    (if s then
                        colors.selected

                     else
                        colors.buttonBackground
                    )
                ]
                { onPress = Just m
                , label = text t
                }
    in
    row
        [ width fill
        , Background.color colors.toolbarBackground
        , Border.rounded 5
        , padding 10
        , spacing 10
        , Font.family [ Font.monospace ]
        , Font.color colors.text
        , Font.size 14
        ]
        [ column [ spacing 5 ]
            (Dict.values sharedState.cars
                |> List.map carInfo
            )
        , column [ alignRight, alignTop, spacing 10 ]
            [ controlLabel "Simulation control"
            , controlButton simulationStateAsText ToggleSimulation False
            , controlLabel "Simulation speed"
            , row [ spacing 10 ]
                [ controlButton "Slow" (SetSimulationSpeed Slow) (sharedState.simulationSpeed == Slow)
                , controlButton "Medium" (SetSimulationSpeed Medium) (sharedState.simulationSpeed == Medium)
                , controlButton "Fast" (SetSimulationSpeed Fast) (sharedState.simulationSpeed == Fast)
                ]
            ]
        ]


carInfo : Car -> Element msg
carInfo car =
    let
        showCarKind =
            image [ width (px 14) ] { description = "", src = "assets/" ++ Car.asset car }

        status =
            case car.status of
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
    in
    row
        [ centerY, spacing 10 ]
        [ showCarKind
        , text (String.join " | " [ Coords.toString car.coords, status ])
        ]
