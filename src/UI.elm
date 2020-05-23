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
        , alignTop
        , column
        , el
        , fill
        , fillPortion
        , height
        , image
        , mouseOver
        , padding
        , paddingXY
        , px
        , rgb255
        , row
        , scrollbarY
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
import SharedState exposing (Dimensions, SharedState, SharedStateUpdate, SimulationSpeed(..), SimulationState(..))
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
    | SetSimulationState SimulationState


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

        SetSimulationState state ->
            ( model, Cmd.none, SharedState.UpdateSimulationState state )


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


editor : SharedState -> Model -> Element Msg
editor sharedState model =
    let
        tileSize =
            sharedState.dimensions.tileSize

        size =
            px (Config.boardSize * floor tileSize)

        rg =
            List.range 1 Config.boardSize

        col y =
            List.map (\x -> tileOverlay sharedState.board tileSize model.selectedTool ( x, y )) rg

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
        [ mouseOver [ Border.innerGlow glowColor tileSize ]
        , width size
        , height size
        ]
        (column [] rows)


tileOverlay : Board -> Float -> Tool -> Coords -> Element Msg
tileOverlay board tileSize selectedTool coords =
    let
        tileSizePx =
            px (floor tileSize)

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
        , mouseOver [ Border.innerGlow glowColor (tileSize / 4) ]
        , Events.onClick (SelectTile coords)
        ]
        Element.none


toolbar : Model -> Dimensions -> Element Msg
toolbar model dimensions =
    let
        buttonGroup buttons =
            wrappedRow
                [ width fill
                , spacing whitespace.tight
                ]
                buttons

        constructionButtonGroup g =
            g
                |> List.map Construction
                |> List.map (toolbarButton dimensions model.selectedTool)
                |> buttonGroup
    in
    column
        [ width (px dimensions.toolbar)
        , alignTop
        , paddingXY whitespace.tight whitespace.regular
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
        , spacing whitespace.regular
        ]
        [ constructionButtonGroup constructionTileGroups.main
        , constructionButtonGroup constructionTileGroups.intersectionCross
        , constructionButtonGroup constructionTileGroups.intersectionT
        , constructionButtonGroup constructionTileGroups.curve
        , constructionButtonGroup constructionTileGroups.deadend
        , buttonGroup
            [ toolbarButton dimensions model.selectedTool Bulldozer
            , toolbarButton dimensions model.selectedTool Dynamite
            , toolbarButton dimensions model.selectedTool IntersectionDesigner
            ]
        ]


toolbarButton : Dimensions -> Tool -> Tool -> Element Msg
toolbarButton dimensions selectedTool tool =
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
            image [ width fill ] { description = "", src = "assets/" ++ asset }
    in
    Input.button
        [ Background.color colors.buttonBackground
        , width (px dimensions.toolbarButton)
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
        , Font.size sharedState.dimensions.text
        , alignTop
        , width (px sharedState.dimensions.menu)
        , paddingXY whitespace.tight whitespace.regular
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
simulationControl { simulationState, dimensions } =
    let
        isSelected speed =
            case simulationState of
                Simulation currentSpeed ->
                    speed == currentSpeed

                Paused ->
                    False
    in
    row
        [ width fill
        , spacing whitespace.tight
        ]
        [ controlButton dimensions "⏸️" (SetSimulationState Paused) (simulationState == Paused)
        , controlButton dimensions "🐌" (SetSimulationState (Simulation Slow)) (isSelected Slow)
        , controlButton dimensions "🐇" (SetSimulationState (Simulation Medium)) (isSelected Medium)
        , controlButton dimensions "🐆" (SetSimulationState (Simulation Fast)) (isSelected Fast)
        ]


controlButton : Dimensions -> String -> Msg -> Bool -> Element Msg
controlButton dimensions label msg selected =
    Input.button
        [ Background.color colors.buttonBackground
        , width (fillPortion 1)
        , Font.size dimensions.menuButton
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


debug : SharedState -> Element Msg
debug sharedState =
    column [ spacing whitespace.tight, width fill ]
        (Dict.values sharedState.cars
            |> List.map (carInfo sharedState.dimensions)
        )


carInfo : Dimensions -> Car -> Element msg
carInfo dimensions car =
    let
        showCarKind =
            image [ width (px dimensions.text) ]
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
