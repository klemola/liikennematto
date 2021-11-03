port module UI.Editor exposing
    ( overlay
    , toolbar
    , update
    )

import CustomEvent
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Maybe.Extra as Maybe
import Message exposing (Message(..))
import Model.Geometry as Geometry
import Model.Liikennematto exposing (Liikennematto, Tool(..))
import Model.RenderCache as RenderCache exposing (refreshTilemapCache)
import Model.Tile as Tile exposing (tileSize)
import Model.Tilemap as Tilemap exposing (Cell)
import Model.World as World exposing (World)
import Task
import UI.Core
    exposing
        ( ControlButtonSize
        , colors
        , controlButton
        , icon
        , whitespace
        )


port playAudio : String -> Cmd msg


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

        AddTile cell ->
            let
                { world } =
                    model

                ( nextTilemap, tileActions ) =
                    world.tilemap |> Tilemap.addTile cell

                nextWorld =
                    { world | tilemap = nextTilemap }
            in
            ( { model
                | world = nextWorld
                , renderCache = refreshTilemapCache nextTilemap model.renderCache
              }
            , Cmd.batch (tileActionsToCmds tileActions)
            )

        RemoveTile cell ->
            let
                { world } =
                    model

                ( nextTilemap, tileActions ) =
                    model.world.tilemap |> Tilemap.removeTile cell

                nextWorld =
                    { world | tilemap = nextTilemap }
            in
            ( { model
                | world = nextWorld
                , renderCache = refreshTilemapCache nextTilemap model.renderCache
              }
            , Cmd.batch (tileActionsToCmds tileActions)
            )

        UpdateTilemap delta ->
            let
                { world } =
                    model

                ( nextTilemap, tileActions, changedCells ) =
                    Tilemap.update delta world.tilemap

                nextWorld =
                    { world | tilemap = nextTilemap }

                ( nextRenderCache, tilemapChangedEffects ) =
                    if List.isEmpty changedCells then
                        ( model.renderCache, Cmd.none )

                    else
                        ( refreshTilemapCache nextTilemap model.renderCache
                        , changedCells
                            |> Task.succeed
                            |> Task.perform TilemapChanged
                        )
            in
            ( { model
                | world = nextWorld
                , renderCache = nextRenderCache
              }
            , Cmd.batch (tilemapChangedEffects :: tileActionsToCmds tileActions)
            )

        ResetWorld ->
            ( { model
                | tool = SmartConstruction
                , world = World.empty
                , renderCache = RenderCache.new World.empty
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


tileActionsToCmds : List Tile.Action -> List (Cmd Message)
tileActionsToCmds =
    List.filterMap
        (\action ->
            case action of
                Tile.PlayAudio sound ->
                    Just (playAudio sound)
        )



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
        , Element.htmlAttribute (CustomEvent.onRightClick <| chooseSecondaryAction cell tool world)
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
            if not alreadyExists && Tilemap.canBuildRoadAt cell world.tilemap then
                AddTile cell

            else
                NoOp

        ( Bulldozer, Just _ ) ->
            let
                tile =
                    Tilemap.tileAt world.tilemap cell
            in
            if Maybe.unwrap False Tile.isBuilt tile then
                RemoveTile cell

            else
                NoOp

        ( Dynamite, _ ) ->
            ResetWorld

        _ ->
            NoOp


chooseSecondaryAction : Cell -> Tool -> World -> Message
chooseSecondaryAction cell tool world =
    case tool of
        SmartConstruction ->
            let
                tile =
                    Tilemap.tileAt world.tilemap cell
            in
            if Maybe.unwrap False Tile.isBuilt tile then
                RemoveTile cell

            else
                NoOp

        _ ->
            NoOp


tileHighlight :
    { world : World
    , selectedTool : Tool
    , cell : Cell
    }
    -> Maybe Color
tileHighlight { world, selectedTool, cell } =
    case selectedTool of
        Bulldozer ->
            if Tilemap.exists cell world.tilemap then
                Just colors.danger

            else
                Nothing

        SmartConstruction ->
            let
                canBuildHere =
                    Tilemap.canBuildRoadAt cell world.tilemap

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
