module UI.Editor exposing
    ( Model
    , Msg
    , initialModel
    , overlay
    , toolbar
    , update
    )

import Config exposing (boardSizeScaled, tileSize)
import CustomEvent
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Model.Cell exposing (Cell)
import Model.World as World exposing (World)
import Pixels
import Quantity
import Simulation.WorldUpdate as WorldUpdate
import UI.Core exposing (ControlButtonSize, colors, controlButton, icon, whitespace)


type Tool
    = SmartConstruction
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
                            |> WorldUpdate.buildRoadAt cell

                      else
                        world
                    , Cmd.none
                    )

                ( Bulldozer, Just _ ) ->
                    ( model
                    , world
                        -- TODO: do this in Simulation instead
                        |> WorldUpdate.removeRoadAt cell
                    , Cmd.none
                    )

                ( Dynamite, _ ) ->
                    ( SmartConstruction
                    , World.empty
                    , Cmd.none
                    )

                _ ->
                    ( model, world, Cmd.none )

        SecondaryAction cell ->
            case model of
                SmartConstruction ->
                    ( model
                    , world
                        -- TODO: do this in Simulation instead
                        |> WorldUpdate.removeRoadAt cell
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
            Element.px (boardSizeScaled |> Pixels.inPixels)

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
        [ Element.mouseOver [ Border.innerGlow highlight (Pixels.inPixels tileSize) ]
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
            Element.px
                (tileSize
                    |> Quantity.floor
                    |> Pixels.inPixels
                )
    in
    Element.el
        [ Element.width tileSizePx
        , Element.height tileSizePx
        , Element.mouseOver
            [ tileSize
                |> Quantity.divideBy 4
                |> Pixels.toFloat
                |> Border.innerGlow glowColor
            ]
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


toolbar : Model -> ControlButtonSize -> Element Msg
toolbar model controlButtonSize =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignLeft
        ]
        [ toolbarButton model SmartConstruction controlButtonSize
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

                Bulldozer ->
                    "bulldozer.png"

                Dynamite ->
                    "dynamite.png"

                None ->
                    "__none__"
    in
    controlButton
        { label = icon asset
        , onPress = SelectTool tool
        , selected = selectedTool == tool
        , disabled = False
        , size = controlButtonSize
        }
