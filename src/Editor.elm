module Editor exposing (Model, Msg, initialModel, overlay, toolbar, update)

import Board exposing (Board)
import Config exposing (borderRadius, borderSize, colors, whitespace)
import CustomEvent
import Element
    exposing
        ( Color
        , Element
        , alignTop
        , column
        , el
        , fill
        , height
        , image
        , mouseOver
        , padding
        , px
        , row
        , spacing
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Position exposing (Position)
import SharedState exposing (Dimensions, Lots, SharedState, SharedStateUpdate)
import Tile exposing (Tile(..))


type Tool
    = SmartConstruction
    | IntersectionDesigner
    | TrafficDirectionDesigner
    | Bulldozer
    | Dynamite
    | None


type alias Model =
    Tool


type Msg
    = SelectTile Position
    | SecondaryAction Position
    | SelectTool Tool


initialModel : Model
initialModel =
    None



-- Update


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SelectTile position ->
            case ( model, Board.get position sharedState.board ) of
                ( SmartConstruction, _ ) ->
                    ( model
                    , Cmd.none
                    , if Board.canBuildRoadAt position sharedState.board then
                        buildRoad sharedState.board position
                            |> SharedState.EditBoardAt position

                      else
                        SharedState.NoUpdate
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , Cmd.none
                    , removeRoad sharedState.board position
                        |> SharedState.EditBoardAt position
                    )

                ( Dynamite, _ ) ->
                    ( SmartConstruction
                    , Cmd.none
                    , SharedState.NewBoard
                    )

                ( IntersectionDesigner, Just tile ) ->
                    ( model
                    , Cmd.none
                    , sharedState.board
                        |> Board.set position (Tile.toggleIntersectionControl tile)
                        |> SharedState.EditTileAt position
                    )

                ( TrafficDirectionDesigner, Just tile ) ->
                    ( model
                    , Cmd.none
                    , sharedState.board
                        |> Board.set position (Tile.toggleTrafficDirection tile)
                        |> SharedState.EditTileAt position
                    )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        SecondaryAction position ->
            case model of
                SmartConstruction ->
                    ( model
                    , Cmd.none
                    , removeRoad sharedState.board position
                        |> SharedState.EditBoardAt position
                    )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        SelectTool tool ->
            let
                nextTool =
                    if model == tool then
                        None

                    else
                        tool
            in
            ( nextTool, Cmd.none, SharedState.NoUpdate )


buildRoad : Board -> Position -> Board
buildRoad board origin =
    let
        boardWithNewTile =
            Board.set origin Config.defaultTile board
    in
    Board.applyMask boardWithNewTile


removeRoad : Board -> Position -> Board
removeRoad board origin =
    let
        boardWithoutTile =
            Board.remove origin board
    in
    Board.applyMask boardWithoutTile



-- Views


overlay : SharedState -> Model -> Element Msg
overlay sharedState model =
    let
        tileSize =
            sharedState.dimensions.tileSize

        size =
            px (Config.boardSize * floor tileSize)

        rg =
            List.range 1 Config.boardSize

        cell x y =
            tileOverlay
                { tileSize = tileSize
                , glowColor =
                    tileHighlight
                        { board = sharedState.board
                        , lots = sharedState.lots
                        , selectedTool = model
                        , position = Position.fromIntegers ( x, y )
                        }
                , position = Position.fromIntegers ( x, y )
                }

        rows =
            List.map
                (\y ->
                    row [] (List.map (\x -> cell x y) rg)
                )
                rg

        highlight =
            case model of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    el
        [ mouseOver [ Border.innerGlow highlight tileSize ]
        , width size
        , height size
        ]
        (column [] rows)


tileOverlay :
    { tileSize : Float
    , glowColor : Color
    , position : Position
    }
    -> Element Msg
tileOverlay { tileSize, glowColor, position } =
    let
        tileSizePx =
            px (floor tileSize)
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow glowColor <| tileSize / 4 ]
        , Events.onClick (SelectTile position)
        , Element.htmlAttribute (CustomEvent.onRightClick <| SecondaryAction position)
        ]
        Element.none


tileHighlight :
    { board : Board
    , lots : Lots
    , selectedTool : Tool
    , position : Position
    }
    -> Color
tileHighlight { board, lots, selectedTool, position } =
    case selectedTool of
        None ->
            colors.transparent

        Bulldozer ->
            colors.danger

        Dynamite ->
            colors.transparent

        SmartConstruction ->
            let
                canBuildHere =
                    Board.canBuildRoadAt position board

                mightDestroyLot =
                    SharedState.hasLot lots position
            in
            if canBuildHere && mightDestroyLot then
                colors.danger

            else if canBuildHere then
                colors.target

            else
                colors.notAllowed

        IntersectionDesigner ->
            case Board.get position board of
                Just (Intersection _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed

        TrafficDirectionDesigner ->
            case Board.get position board of
                Just (TwoLaneRoad _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed


toolbar : Model -> Dimensions -> Element Msg
toolbar model dimensions =
    column
        [ width (px dimensions.toolbar)
        , alignTop
        , padding whitespace.tight
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
        , spacing whitespace.tight
        ]
        [ toolbarButton model SmartConstruction
        , toolbarButton model IntersectionDesigner
        , toolbarButton model TrafficDirectionDesigner
        , toolbarButton model Bulldozer
        , toolbarButton model Dynamite
        ]


toolbarButton : Tool -> Tool -> Element Msg
toolbarButton selectedTool tool =
    let
        asset =
            case tool of
                SmartConstruction ->
                    "smart_construction.png"

                IntersectionDesigner ->
                    "intersection_designer.png"

                TrafficDirectionDesigner ->
                    "traffic_direction_designer.png"

                Bulldozer ->
                    "bulldozer.png"

                Dynamite ->
                    "dynamite.png"

                None ->
                    "__none__"

        show =
            image [ width fill ] { description = "", src = "assets/" ++ asset }
    in
    Input.button
        [ Background.color colors.buttonBackground
        , width fill
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
