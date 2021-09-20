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
import Model.Cell exposing (Cell)
import Model.Liikennematto exposing (Liikennematto, Tool(..))
import Model.World as World exposing (World)
import Pixels
import Quantity
import Simulation.WorldUpdate as WorldUpdate
import UI.Core exposing (ControlButtonSize, colors, controlButton, icon, whitespace)



-- Update


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        SelectTile cell ->
            case ( model.tool, World.tileAt cell model.world ) of
                ( SmartConstruction, _ ) ->
                    ( { model
                        | world =
                            if World.canBuildRoadAt cell model.world then
                                model.world |> WorldUpdate.buildRoadAt cell

                            else
                                model.world
                      }
                    , Cmd.none
                    )

                ( Bulldozer, Just _ ) ->
                    ( { model
                        | world =
                            model.world
                                -- TODO: do this in Simulation instead
                                |> WorldUpdate.removeRoadAt cell
                      }
                    , Cmd.none
                    )

                ( Dynamite, _ ) ->
                    ( { model | tool = SmartConstruction, world = World.empty }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SecondaryAction cell ->
            case model.tool of
                SmartConstruction ->
                    ( { model
                        | world =
                            model.world
                                -- TODO: do this in Simulation instead
                                |> WorldUpdate.removeRoadAt cell
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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


overlay : Liikennematto -> Element Message
overlay model =
    let
        size =
            Element.px (boardSizeScaled |> Pixels.inPixels)

        rg =
            List.range 1 Config.boardSize

        cell x y =
            tileOverlay
                { glowColor =
                    tileHighlight
                        { world = model.world
                        , selectedTool = model.tool
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
            case model.tool of
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
    -> Element Message
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


toolbar : Liikennematto -> ControlButtonSize -> Element Message
toolbar { tool } controlButtonSize =
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
