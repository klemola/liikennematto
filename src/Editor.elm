module Editor exposing (Model, Msg, initialModel, overlay, toolbar, update)

import Cell exposing (Cell)
import Config
    exposing
        ( colors
        , tileSize
        , whitespace
        )
import CustomEvent
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Tile exposing (Tile(..))
import UI exposing (ControlButtonSize)
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
    SmartConstruction



-- Update


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
        SelectTile cell ->
            case ( model, World.tileAt cell world ) of
                ( SmartConstruction, _ ) ->
                    ( model
                    , if World.canBuildRoadAt cell world then
                        world
                            |> World.buildRoadAt cell

                      else
                        world
                    , Cmd.none
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , world
                        |> World.removeRoadAt cell
                    , Cmd.none
                    )

                ( Dynamite, _ ) ->
                    ( SmartConstruction
                    , world
                        |> World.reset
                    , Cmd.none
                    )

                ( IntersectionDesigner, Just tile ) ->
                    ( model
                    , world
                        |> World.withTileAt cell (Tile.toggleIntersectionControl tile)
                    , Cmd.none
                    )

                ( TrafficDirectionDesigner, Just tile ) ->
                    ( model
                    , world
                        |> World.withTileAt cell (Tile.toggleTrafficDirection tile)
                    , Cmd.none
                    )

                _ ->
                    ( model, world, Cmd.none )

        SecondaryAction cell ->
            case model of
                SmartConstruction ->
                    ( model
                    , world
                        |> World.removeRoadAt cell
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
            Element.px (Config.boardSize * floor tileSize)

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
                    Element.row [] (List.map (\x -> cell x y) rg)
                )
                rg

        highlight =
            case model of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    Element.el
        [ Element.mouseOver [ Border.innerGlow highlight tileSize ]
        , Element.width size
        , Element.height size
        ]
        (Element.column [] rows)


tileOverlay :
    { glowColor : Color
    , cell : Cell
    }
    -> Element Msg
tileOverlay { glowColor, cell } =
    let
        tileSizePx =
            Element.px (floor tileSize)
    in
    Element.el
        [ Element.width tileSizePx
        , Element.height tileSizePx
        , Element.mouseOver [ Border.innerGlow glowColor <| tileSize / 4 ]
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
            case World.tileAt cell world of
                Just (Intersection _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed

        TrafficDirectionDesigner ->
            case World.tileAt cell world of
                Just (TwoLaneRoad _ _) ->
                    colors.target

                _ ->
                    colors.notAllowed


toolbar : Model -> ControlButtonSize -> Element Msg
toolbar model controlButtonSize =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignLeft
        ]
        [ toolbarButton model SmartConstruction controlButtonSize
        , toolbarButton model IntersectionDesigner controlButtonSize
        , toolbarButton model TrafficDirectionDesigner controlButtonSize
        , toolbarButton model Bulldozer controlButtonSize
        , toolbarButton model Dynamite controlButtonSize
        ]


toolbarButton : Tool -> Tool -> ControlButtonSize -> Element Msg
toolbarButton selectedTool tool controlButtonSize =
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
    in
    UI.controlButton
        { label = UI.icon asset
        , onPress = SelectTool tool
        , selected = selectedTool == tool
        , size = controlButtonSize
        }
