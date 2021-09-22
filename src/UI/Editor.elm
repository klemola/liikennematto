module UI.Editor exposing
    ( overlay
    , toolbar
    , update
    )

import Config exposing (boardSizeScaled, tileSize)
import CustomEvent
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Message exposing (Message(..))
import Model.Board as Board
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (Liikennematto, Tool(..))
import Model.World as World exposing (World)
import Pixels
import Quantity
import UI.Core
    exposing
        ( ControlButtonSize
        , colors
        , controlButton
        , icon
        , whitespace
        )



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
            Element.px (boardSizeScaled |> Pixels.inPixels)

        rg =
            List.range 1 Config.boardSize

        cell x y =
            tileOverlay
                { glowColor =
                    tileHighlight
                        { world = world
                        , selectedTool = tool
                        , cell = ( x, y )
                        }
                , cell = ( x, y )
                , world = world
                , tool = tool
                }

        rows =
            List.map
                (\y ->
                    Element.row [] (List.map (\x -> cell x y) rg)
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
        [ Element.mouseOver [ Border.innerGlow highlight (Pixels.inPixels tileSize) ]
        , Element.width size
        , Element.height size
        ]
        (Element.column [] rows)


tileOverlay :
    { glowColor : Color
    , cell : Cell
    , world : World
    , tool : Tool
    }
    -> Element Message
tileOverlay { glowColor, cell, world, tool } =
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
        , Events.onClick (choosePrimaryAction cell tool world)
        , Element.htmlAttribute (CustomEvent.onRightClick <| chooseSecondaryAction cell tool)
        ]
        Element.none


choosePrimaryAction : Cell -> Tool -> World -> Message
choosePrimaryAction cell tool world =
    case ( tool, World.tileAt cell world ) of
        ( SmartConstruction, _ ) ->
            let
                alreadyExists =
                    Board.exists cell world.board
            in
            if not alreadyExists && World.canBuildRoadAt cell world then
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
