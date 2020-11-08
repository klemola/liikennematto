module Editor exposing (Model, Msg, initialModel, overlay, toolbar, update)

import Cell exposing (Cell)
import Config exposing (borderRadius, borderSize, colors, tileSize, whitespace)
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
import Tile exposing (Tile(..))
import World exposing (World)


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
    = SelectTile Cell
    | SecondaryAction Cell
    | SelectTool Tool


initialModel : Model
initialModel =
    None



-- Update


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
        SelectTile cell ->
            case ( model, World.getCellContents cell world ) of
                ( SmartConstruction, _ ) ->
                    ( model
                    , if World.canBuildRoadAt cell world then
                        world
                            |> World.withNewRoad cell

                      else
                        world
                    , Cmd.none
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , world
                        |> World.withEmptyCell cell
                    , Cmd.none
                    )

                ( Dynamite, _ ) ->
                    ( SmartConstruction
                    , World.withEmptyBoard world
                    , Cmd.none
                    )

                ( IntersectionDesigner, Just tile ) ->
                    ( model
                    , world
                        |> World.withUpdatedCell cell (Tile.toggleIntersectionControl tile)
                    , Cmd.none
                    )

                ( TrafficDirectionDesigner, Just tile ) ->
                    ( model
                    , world
                        |> World.withUpdatedCell cell (Tile.toggleTrafficDirection tile)
                    , Cmd.none
                    )

                _ ->
                    ( model, world, Cmd.none )

        SecondaryAction cell ->
            case model of
                SmartConstruction ->
                    ( model
                    , world
                        |> World.withEmptyCell cell
                    , Cmd.none
                    )

                _ ->
                    ( model, world, Cmd.none )

        SelectTool tool ->
            let
                nextTool =
                    if model == tool then
                        None

                    else
                        tool
            in
            ( nextTool, world, Cmd.none )



-- Views


overlay : World -> Model -> Element Msg
overlay world model =
    let
        size =
            px (Config.boardSize * floor tileSize)

        rg =
            List.range 1 Config.boardSize

        cell x y =
            tileOverlay
                { glowColor =
                    tileHighlight
                        { world = world
                        , selectedTool = model
                        , cell = ( x, y )
                        }
                , cell = ( x, y )
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
    { glowColor : Color
    , cell : Cell
    }
    -> Element Msg
tileOverlay { glowColor, cell } =
    let
        tileSizePx =
            px (floor tileSize)
    in
    el
        [ width tileSizePx
        , height tileSizePx
        , mouseOver [ Border.innerGlow glowColor <| tileSize / 4 ]
        , Events.onClick (SelectTile cell)
        , Element.htmlAttribute (CustomEvent.onRightClick <| SecondaryAction cell)
        ]
        Element.none


tileHighlight :
    { world : World
    , selectedTool : Tool
    , cell : Cell
    }
    -> Color
tileHighlight { world, selectedTool, cell } =
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
                    World.canBuildRoadAt cell world

                mightDestroyLot =
                    World.hasLot cell world
            in
            if canBuildHere && mightDestroyLot then
                colors.danger

            else if canBuildHere then
                colors.target

            else
                colors.notAllowed

        IntersectionDesigner ->
            case World.getCellContents cell world of
                Just (Intersection _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed

        TrafficDirectionDesigner ->
            case World.getCellContents cell world of
                Just (TwoLaneRoad _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed


toolbar : Model -> Int -> Element Msg
toolbar model currentWidth =
    column
        [ width (px currentWidth)
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
