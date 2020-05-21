module UI exposing (Model, Msg(..), borderRadius, borderSize, colors, editor, initialModel, menu, toolbar, update, whitespace)

import Board exposing (Board)
import Car exposing (Car)
import Config exposing (constructionTileGroups)
import Coords exposing (Coords)
import Dict
import Direction exposing (Orientation(..))
import Element
    exposing
        ( Element
        , centerX
        , column
        , el
        , fill
        , height
        , image
        , minimum
        , mouseOver
        , padding
        , paddingXY
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate, SimulationSpeed(..), SimulationState(..))
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..))


type Tool
    = Construction Tile
    | IntersectionDesigner
    | Bulldozer
    | Dynamite
    | None


type alias Model =
    { selectedTool : Tool
    }


type Msg
    = SelectTile Coords
    | SelectTool Tool
    | ToggleSimulation
    | SetSimulationSpeed SimulationSpeed


initialModel : Model
initialModel =
    { selectedTool = None
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SelectTile coords ->
            case ( model.selectedTool, Board.get sharedState.board coords ) of
                ( Construction tile, _ ) ->
                    ( model
                    , Cmd.none
                    , Board.set sharedState.board coords tile
                        |> SharedState.EditBoardAt coords
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , Cmd.none
                    , Board.remove coords sharedState.board
                        |> SharedState.EditBoardAt coords
                    )

                ( Dynamite, _ ) ->
                    ( { model | selectedTool = None }
                    , Cmd.none
                    , SharedState.NewBoard
                    )

                ( IntersectionDesigner, Just tile ) ->
                    ( model
                    , Cmd.none
                    , Tile.toggleIntersectionControl tile
                        |> Board.set sharedState.board coords
                        |> SharedState.EditBoardAt coords
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

        ToggleSimulation ->
            let
                nextSimulationState =
                    SharedState.nextSimulationState sharedState.simulationState
            in
            ( model, Cmd.none, SharedState.UpdateSimulationState nextSimulationState )

        SetSimulationSpeed speed ->
            ( model, Cmd.none, SharedState.UpdateSimulationSpeed speed )


colors =
    { mainBackground = rgb255 68 115 120
    , toolbarBackground = rgb255 159 192 198
    , buttonBackground = rgb255 228 228 235
    , listItemBackground = rgb255 109 151 156
    , text = rgb255 52 65 67
    , textInverse = rgb255 222 222 222
    , selected = rgb255 242 212 13
    , danger = rgb255 235 119 52
    , notAllowed = rgb255 245 66 84
    , target = rgb255 222 222 222
    , terrain = rgb255 33 191 154
    , transparent = Element.rgba255 0 0 0 0
    , lightBorder = rgb255 220 220 226
    , heavyBorder = rgb255 53 93 97
    }


whitespace =
    { regular = 10
    , spacious = 20
    , tight = 5
    }


borderSize =
    { heavy = 10
    , light = 3
    }


borderRadius =
    { heavy = 5
    , light = 3
    }


editorWidth : Int
editorWidth =
    Config.boardSize * floor Config.tileSize + (2 * borderSize.heavy)


editor : SharedState -> Model -> Element Msg
editor sharedState model =
    let
        rg =
            List.range 1 Config.boardSize

        col y =
            List.map (\x -> tileOverlay sharedState.board model.selectedTool ( x, y )) rg

        rows =
            List.map (\y -> row [] (col y)) rg

        glowColor =
            case model.selectedTool of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    el
        [ mouseOver [ Border.innerGlow glowColor Config.tileSize ]
        , width (px editorWidth)
        , height (px editorWidth)
        ]
        (column [] rows)


tileOverlay : Board -> Tool -> Coords -> Element Msg
tileOverlay board selectedTool coords =
    let
        tileSizePx =
            px (floor Config.tileSize)

        glowColor =
            case selectedTool of
                None ->
                    colors.transparent

                Bulldozer ->
                    colors.danger

                Dynamite ->
                    colors.transparent

                Construction tile ->
                    if Board.canAddTile board coords tile then
                        colors.target

                    else
                        colors.notAllowed

                IntersectionDesigner ->
                    case Board.get board coords of
                        Just (Intersection _ _) ->
                            colors.target

                        _ ->
                            colors.notAllowed
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow glowColor (Config.tileSize / 4) ]
        , Events.onClick (SelectTile coords)
        ]
        Element.none


toolbar : Model -> Element Msg
toolbar model =
    let
        buttonGroup buttons =
            if List.length buttons > 2 then
                wrappedRow [ width (px 102), spacing whitespace.tight ] buttons

            else
                row [ spacing whitespace.tight ] buttons

        constructionButtonGroup g =
            g
                |> List.map Construction
                |> List.map (toolbarButton model.selectedTool)
                |> buttonGroup
    in
    el
        [ width (fill |> minimum 120)
        , height fill
        , padding whitespace.regular
        , Background.color colors.toolbarBackground
        , Border.rounded borderRadius.heavy
        , Border.solid
        , Border.widthEach
            { top = borderSize.heavy
            , bottom = borderSize.heavy
            , left = borderSize.light
            , right = borderSize.light
            }
        , Border.color colors.heavyBorder
        ]
        (column
            [ spacing (whitespace.regular * 2)
            , centerX
            ]
            [ constructionButtonGroup constructionTileGroups.main
            , constructionButtonGroup constructionTileGroups.intersectionCross
            , constructionButtonGroup constructionTileGroups.intersectionT
            , constructionButtonGroup constructionTileGroups.curve
            , constructionButtonGroup constructionTileGroups.deadend
            , buttonGroup
                [ toolbarButton model.selectedTool Bulldozer
                , toolbarButton model.selectedTool Dynamite
                , toolbarButton model.selectedTool IntersectionDesigner
                ]
            ]
        )


toolbarButton : Tool -> Tool -> Element Msg
toolbarButton selectedTool tool =
    let
        asset =
            case tool of
                Construction (TwoLaneRoad kind) ->
                    Tile.roadAsset kind

                Construction (Intersection _ shape) ->
                    Tile.intersectionAsset shape

                IntersectionDesigner ->
                    "intersection_designer.png"

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
        , Border.width borderSize.light
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.color
            (if selectedTool == tool then
                colors.selected

             else
                colors.lightBorder
            )
        ]
        { onPress = Just (SelectTool tool)
        , label = show
        }


menu : SharedState -> Element Msg
menu sharedState =
    column
        [ Font.family [ Font.monospace ]
        , Font.color colors.text
        , Font.size 14
        , height fill
        , padding whitespace.tight
        , spacing whitespace.regular
        , Background.color colors.toolbarBackground
        , Border.rounded borderRadius.heavy
        , Border.solid
        , Border.widthEach
            { top = borderSize.heavy
            , bottom = borderSize.heavy
            , left = borderSize.light
            , right = borderSize.light
            }
        , Border.color colors.heavyBorder
        ]
        [ simulationControl sharedState
        , debug sharedState
        ]


simulationControl : SharedState -> Element Msg
simulationControl sharedState =
    let
        simulationStateAsText =
            case sharedState.simulationState of
                Simulation ->
                    "Pause simulation"

                Paused ->
                    "Resume simulation"

        label t =
            el [ Font.semiBold ] (text t)
    in
    column
        [ spacing whitespace.regular
        , paddingXY whitespace.tight whitespace.regular
        ]
        [ button simulationStateAsText ToggleSimulation False
        , label "Simulation speed"
        , row [ spacing whitespace.tight ]
            [ button "Slow" (SetSimulationSpeed Slow) (sharedState.simulationSpeed == Slow)
            , button "Medium" (SetSimulationSpeed Medium) (sharedState.simulationSpeed == Medium)
            , button "Fast" (SetSimulationSpeed Fast) (sharedState.simulationSpeed == Fast)
            ]
        ]


debug : SharedState -> Element Msg
debug sharedState =
    column [ spacing whitespace.tight, width fill ]
        (Dict.values sharedState.cars
            |> List.map carInfo
        )


carInfo : Car -> Element msg
carInfo car =
    let
        showCarKind =
            image [ width (px 14) ]
                { description = ""
                , src = "assets/" ++ Car.asset car
                }
    in
    row
        [ width fill
        , padding whitespace.tight
        , spacing whitespace.regular
        , Font.color colors.textInverse
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ showCarKind
        , column [ spacing whitespace.tight ]
            [ text (Coords.toString car.coords)
            , text (Car.statusDescription car.status)
            ]
        ]


type alias Button =
    Element Msg


button : String -> Msg -> Bool -> Button
button label msg selected =
    Input.button
        [ Background.color colors.buttonBackground
        , padding whitespace.tight
        , Border.width borderSize.light
        , Border.rounded borderRadius.light
        , Border.solid
        , Border.color
            (if selected then
                colors.selected

             else
                colors.lightBorder
            )
        ]
        { onPress = Just msg
        , label = text label
        }
