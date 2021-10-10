module UI.Editor exposing
    ( overlay
    , toolbar
    , update
    )

import CustomEvent
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Message exposing (Message(..))
import Model.Geometry as Geometry
import Model.Liikennematto exposing (Liikennematto, Tool(..))
import Model.Tilemap as Tilemap exposing (Cell, tileSize)
import Model.World as World exposing (World)
import Simulation.Infrastructure as Infrastructure
import UI.Core
    exposing
        ( ControlButtonSize
        , colors
        , controlButton
        , icon
        , whitespace
        )


tileSizeInPixelsFloat : Float
tileSizeInPixelsFloat =
    Geometry.toPixelsValue tileSize


tileSizeInPixelsInt : Int
tileSizeInPixelsInt =
    tileSizeInPixelsFloat |> floor



-- Update


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        SelectTool tool ->
            let
                nextTool =
                    if model.tool == tool then
                        None

                    else
                        tool
            in
            ( { model | tool = nextTool }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- Views


overlay : World -> Tool -> Element Message
overlay world tool =
    let
        size =
            Element.px (Geometry.toPixelsValue Tilemap.mapSize |> floor)

        rg =
            List.range 1 Tilemap.rowsAndColumnsAmount

        cellElement x y =
            case Tilemap.cellFromCoordinates ( x, y ) of
                Just cell ->
                    tileOverlay
                        { glowColor =
                            tileHighlight
                                { world = world
                                , selectedTool = tool
                                , cell = cell
                                }
                        , cell = cell
                        , world = world
                        , tool = tool
                        }

                Nothing ->
                    Element.none

        rows =
            List.map
                (\y ->
                    Element.row [] (List.map (\x -> cellElement x y) rg)
                )
                rg

        highlight =
            case tool of
                Dynamite ->
                    colors.danger

                _ ->
                    colors.transparent
    in
    Element.el
        [ Element.mouseOver [ Border.innerGlow highlight tileSizeInPixelsFloat ]
        , Element.width size
        , Element.height size
        ]
        (Element.column [] rows)


tileOverlay :
    { glowColor : Maybe Color
    , cell : Cell
    , world : World
    , tool : Tool
    }
    -> Element Message
tileOverlay { glowColor, cell, world, tool } =
    let
        tileSizePx =
            Element.px tileSizeInPixelsInt

        glow =
            glowColor
                |> Maybe.map
                    (\color ->
                        [ Border.innerGlow color (tileSizeInPixelsFloat / 10) ]
                    )
                |> Maybe.withDefault []
    in
    Element.el
        [ Element.width tileSizePx
        , Element.height tileSizePx
        , Element.mouseOver glow
        , Events.onClick (choosePrimaryAction cell tool world)
        , Element.htmlAttribute (CustomEvent.onRightClick <| chooseSecondaryAction cell tool)
        ]
        Element.none


choosePrimaryAction : Cell -> Tool -> World -> Message
choosePrimaryAction cell tool world =
    case ( tool, Tilemap.tileAt world.tilemap cell ) of
        ( SmartConstruction, _ ) ->
            let
                alreadyExists =
                    Tilemap.exists cell world.tilemap
            in
            if not alreadyExists && Infrastructure.canBuildRoadAt cell world then
                AddTile cell

            else
                NoOp

        ( Bulldozer, Just _ ) ->
            RemoveTile cell

        ( Dynamite, _ ) ->
            ResetWorld

        _ ->
            NoOp


chooseSecondaryAction : Cell -> Tool -> Message
chooseSecondaryAction cell tool =
    case tool of
        SmartConstruction ->
            RemoveTile cell

        _ ->
            NoOp


tileHighlight :
    { world : World
    , selectedTool : Tool
    , cell : Cell
    }
    -> Maybe Color
tileHighlight { world, selectedTool, cell } =
    case ( selectedTool, Tilemap.exists cell world.tilemap ) of
        ( Bulldozer, True ) ->
            Just colors.danger

        ( SmartConstruction, _ ) ->
            let
                canBuildHere =
                    Infrastructure.canBuildRoadAt cell world

                mightDestroyLot =
                    World.hasLot cell world
            in
            if canBuildHere && mightDestroyLot then
                Just colors.danger

            else if canBuildHere then
                Just colors.target

            else
                Just colors.notAllowed

        _ ->
            Nothing


toolbar : Tool -> ControlButtonSize -> Element Message
toolbar tool controlButtonSize =
    Element.row
        [ Element.spacing whitespace.tight
        , Element.alignLeft
        ]
        [ toolbarButton tool SmartConstruction controlButtonSize
        , toolbarButton tool Bulldozer controlButtonSize
        , toolbarButton tool Dynamite controlButtonSize
        ]


toolbarButton : Tool -> Tool -> ControlButtonSize -> Element Message
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
