module Editor exposing (Model, Msg, initialModel, overlay, toolbar, update)

import Board exposing (Board)
import Config exposing (borderRadius, borderSize, colors, whitespace)
import Coords exposing (Coords)
import CustomEvent
import Element
    exposing
        ( Element
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
import SharedState exposing (Dimensions, SharedState, SharedStateUpdate)
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
    = SelectTile Coords
    | SecondaryAction Coords
    | SelectTool Tool


initialModel : Model
initialModel =
    None



-- Update


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SelectTile coords ->
            case ( model, Board.get coords sharedState.board ) of
                ( SmartConstruction, _ ) ->
                    ( model
                    , Cmd.none
                    , if Board.canBuildRoadAt coords sharedState.board then
                        buildRoad sharedState.board coords
                            |> SharedState.EditBoardAt coords

                      else
                        SharedState.NoUpdate
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , Cmd.none
                    , removeRoad sharedState.board coords
                        |> SharedState.EditBoardAt coords
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
                        |> Board.set coords (Tile.toggleIntersectionControl tile)
                        |> SharedState.EditBoardAt coords
                    )

                ( TrafficDirectionDesigner, Just tile ) ->
                    ( model
                    , Cmd.none
                    , sharedState.board
                        |> Board.set coords (Tile.toggleTrafficDirection tile)
                        |> SharedState.EditBoardAt coords
                    )

                _ ->
                    ( model, Cmd.none, SharedState.NoUpdate )

        SecondaryAction coords ->
            case model of
                SmartConstruction ->
                    ( model
                    , Cmd.none
                    , removeRoad sharedState.board coords
                        |> SharedState.EditBoardAt coords
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


buildRoad : Board -> Coords -> Board
buildRoad board origin =
    let
        boardWithNewTile =
            Board.set origin Config.defaultTile board
    in
    Board.applyMask boardWithNewTile


removeRoad : Board -> Coords -> Board
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

        col y =
            List.map (\x -> tileOverlay sharedState.board tileSize model ( x, y )) rg

        rows =
            List.map (\y -> row [] (col y)) rg

        glowColor =
            case model of
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

                SmartConstruction ->
                    if Board.canBuildRoadAt coords board then
                        colors.target

                    else
                        colors.notAllowed

                IntersectionDesigner ->
                    case Board.get coords board of
                        Just (Intersection _ _) ->
                            colors.target

                        _ ->
                            colors.notAllowed

                TrafficDirectionDesigner ->
                    case Board.get coords board of
                        Just (TwoLaneRoad _ _) ->
                            colors.target

                        _ ->
                            colors.notAllowed
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow glowColor <| tileSize / 4 ]
        , Events.onClick (SelectTile coords)
        , Element.htmlAttribute (CustomEvent.onRightClick <| SecondaryAction coords)
        ]
        Element.none


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
