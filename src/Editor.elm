module Editor exposing (Model, Msg, initialModel, overlay, toolbar, update)

import Board exposing (Board)
import Config exposing (borderRadius, borderSize, colors, constructionTileGroups, whitespace)
import Coords exposing (Coords)
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
        , paddingXY
        , px
        , row
        , spacing
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Graphics
import SharedState exposing (Dimensions, SharedState, SharedStateUpdate)
import Tile exposing (IntersectionControl(..), RoadKind(..), Tile(..))


type Tool
    = Construction Tile
    | IntersectionDesigner
    | Bulldozer
    | Dynamite
    | None


type alias Model =
    Tool


type Msg
    = SelectTile Coords
    | SelectTool Tool


initialModel : Model
initialModel =
    None


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        SelectTile coords ->
            case ( model, Board.get coords sharedState.board ) of
                ( Construction tile, _ ) ->
                    ( model
                    , Cmd.none
                    , sharedState.board
                        |> Board.set coords tile
                        |> SharedState.EditBoardAt coords
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , Cmd.none
                    , Board.remove coords sharedState.board
                        |> SharedState.EditBoardAt coords
                    )

                ( Dynamite, _ ) ->
                    ( None
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

                Construction tile ->
                    if Board.canAddTile coords tile board then
                        colors.target

                    else
                        colors.notAllowed

                IntersectionDesigner ->
                    case Board.get coords board of
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
                |> List.map (toolbarButton dimensions model)
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
            [ toolbarButton dimensions model Bulldozer
            , toolbarButton dimensions model Dynamite
            , toolbarButton dimensions model IntersectionDesigner
            ]
        ]


toolbarButton : Dimensions -> Tool -> Tool -> Element Msg
toolbarButton dimensions selectedTool tool =
    let
        asset =
            case tool of
                Construction (TwoLaneRoad kind _) ->
                    Graphics.roadAsset kind

                Construction (Intersection _ shape) ->
                    Graphics.intersectionAsset shape

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
