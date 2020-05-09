module UI exposing (..)

import Board
import Car exposing (Car, Status(..), TurnKind(..))
import Config exposing (constructionTileGroups)
import Coords exposing (Coords)
import Dict
import Direction exposing (Orientation(..))
import Element exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, mouseOver, padding, px, rgb255, row, scrollbarX, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate, SimulationSpeed(..), SimulationState(..))
import Tile exposing (RoadKind(..), Tile(..))


type Tool
    = Construction Tile
    | Bulldozer
    | Dynamite
    | None


type alias Model =
    { selectedTool : Tool
    }


type Msg
    = ToggleSimulation
    | SetSimulationSpeed SimulationSpeed
    | SelectTile Coords
    | SelectTool Tool


initialModel : Model
initialModel =
    { selectedTool = None
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
                ( Construction tile, _ ) ->
                    ( model
                    , Cmd.none
                    , Board.set coords tile sharedState.board
                        |> SharedState.UpdateBoard
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , Cmd.none
                    , Board.remove coords sharedState.board
                        |> SharedState.UpdateBoard
                    )

                ( Dynamite, _ ) ->
                    ( { model | selectedTool = None }
                    , Cmd.none
                    , Board.new
                        |> SharedState.UpdateBoard
                    )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        SelectTool tool ->
            let
                nextTool =
                    if model.selectedTool == tool then
                        None

                    else
                        tool
            in
            ( { model | selectedTool = nextTool }, Cmd.none, SharedState.NoUpdate )


colors =
    { mainBackground = rgb255 33 191 154
    , toolbarBackground = rgb255 191 213 217
    , buttonBackground = rgb255 222 222 222
    , text = rgb255 52 65 67
    , selected = rgb255 245 220 62
    , danger = rgb255 235 119 52
    , grid = rgb255 222 222 222
    , transparent = Element.rgba255 0 0 0 0
    }


whitespace =
    { regular = 10
    , button = 5
    }


view : SharedState -> Model -> Element Msg
view sharedState model =
    column [ width fill, spacing whitespace.regular ]
        [ editor sharedState model
        , toolbar model
        , debug sharedState
        ]


editor : SharedState -> Model -> Element Msg
editor sharedState model =
    let
        rg =
            List.range 1 Config.boardSize

        col y =
            List.map (\x -> tileOverlay model.selectedTool ( x, y )) rg

        rows =
            List.map (\y -> row [] (col y)) rg

        glowColor =
            case model.selectedTool of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    el [ mouseOver [ Border.innerGlow glowColor 5 ] ] (column [] rows)


tileOverlay : Tool -> Coords -> Element Msg
tileOverlay selectedTool coords =
    let
        tileSizePx =
            px (floor Config.tileSize)

        glowColor =
            case selectedTool of
                None ->
                    colors.grid

                Bulldozer ->
                    colors.danger

                Dynamite ->
                    colors.transparent

                _ ->
                    colors.selected
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow glowColor 5 ]
        , Events.onClick (SelectTile coords)
        ]
        Element.none


toolbar : Model -> Element Msg
toolbar model =
    let
        constructionButtonGroup g =
            g
                |> List.map Construction
                |> List.map (toolbarButton model.selectedTool)
                |> buttonGroup
    in
    row
        [ width fill
        , Background.color colors.toolbarBackground
        , Border.rounded whitespace.button
        , padding whitespace.regular
        , spacing (whitespace.regular * 2)
        , scrollbarX
        ]
        [ constructionButtonGroup constructionTileGroups.main
        , constructionButtonGroup constructionTileGroups.intersectionCross
        , constructionButtonGroup constructionTileGroups.intersectionT
        , constructionButtonGroup constructionTileGroups.curve
        , constructionButtonGroup constructionTileGroups.deadend
        , buttonGroup
            [ toolbarButton model.selectedTool Bulldozer
            , toolbarButton model.selectedTool Dynamite
            ]
        ]


toolbarButton : Tool -> Tool -> Element Msg
toolbarButton selectedTool tool =
    let
        asset =
            case tool of
                Construction (TwoLaneRoad kind) ->
                    Tile.roadAsset kind

                Construction (Intersection _ shape) ->
                    Tile.intersectionAsset shape

                Bulldozer ->
                    "bulldozer.png"

                Dynamite ->
                    "dynamite.png"

                _ ->
                    "__none__"

        show =
            image [ width (px 42) ] { description = "", src = "assets/" ++ asset }
    in
    Input.button
        [ Background.color colors.buttonBackground
        , Border.width 3
        , Border.solid
        , Border.rounded 3
        , Border.color
            (if selectedTool == tool then
                colors.selected

             else
                colors.buttonBackground
            )
        ]
        { onPress = Just (SelectTool tool)
        , label = show
        }


buttonGroup : List (Element Msg) -> Element Msg
buttonGroup buttons =
    if List.length buttons > 2 then
        wrappedRow [ width (px 100), spacing whitespace.button ] buttons

    else
        column [ alignTop, spacing whitespace.button ] buttons


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
                , padding whitespace.button
                , Border.width 3
                , Border.rounded 3
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
        , Border.rounded whitespace.button
        , padding whitespace.regular
        , spacing whitespace.regular
        , Font.family [ Font.monospace ]
        , Font.color colors.text
        , Font.size 14
        ]
        [ column [ spacing whitespace.button ]
            (Dict.values sharedState.cars
                |> List.map carInfo
            )
        , column [ alignRight, alignTop, spacing whitespace.regular ]
            [ controlLabel "Simulation control"
            , controlButton simulationStateAsText ToggleSimulation False
            , controlLabel "Simulation speed"
            , row [ spacing whitespace.regular ]
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
        [ centerY, spacing whitespace.regular ]
        [ showCarKind
        , text (String.join " | " [ Coords.toString car.coords, status ])
        ]
