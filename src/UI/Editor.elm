module UI.Editor exposing
    ( overlay
    , toolbar
    , update
    )

import CustomEvent
import Duration
import Element exposing (Color, Element)
import Element.Border as Border
import Element.Events as Events
import Message exposing (Message(..))
import Model.Animation as Animation
import Model.AnimationSchedule as AnimationSchedule exposing (AnimationSchedule)
import Model.Geometry as Geometry
import Model.Liikennematto exposing (Liikennematto, Tool(..), latestTilemap)
import Model.Tilemap as Tilemap exposing (Cell, TilemapChange, tileSize)
import Model.World as World exposing (World)
import Process
import Task
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

        AddTile cell ->
            let
                { world, animationSchedule } =
                    model

                tilemapChange =
                    latestTilemap model |> Tilemap.addTile cell

                nextWorld =
                    { world | tilemap = tilemapChange.nextTilemap }

                nextAnimationSchedule =
                    animateTilemapChange tilemapChange animationSchedule
            in
            ( { model
                | world = nextWorld
                , pendingTilemapChange = Nothing
                , animationSchedule = nextAnimationSchedule
              }
            , tilemapChangedEffects tilemapChange
            )

        RemoveTile cell ->
            let
                tilemapChange =
                    latestTilemap model |> Tilemap.removeTile cell

                nextAnimationSchedule =
                    animateTilemapChange tilemapChange model.animationSchedule
            in
            ( { model
                | -- the tilemap update is delayed so that the animation can complete before the tile is removed
                  -- Room for improvement: if tiles had a state machine, the removal could be implemented as a transition + animation
                  pendingTilemapChange = Just tilemapChange
                , animationSchedule = nextAnimationSchedule
              }
            , tilemapChangedEffects tilemapChange
            )

        _ ->
            ( model, Cmd.none )


animateTilemapChange : TilemapChange -> AnimationSchedule -> AnimationSchedule
animateTilemapChange tilemapChange animationSchedule =
    let
        tileAnimations =
            Animation.fromTilemapChange tilemapChange
    in
    animationSchedule |> AnimationSchedule.add tileAnimations


tilemapChangedEffects : TilemapChange -> Cmd Message
tilemapChangedEffects tilemapChange =
    Process.sleep (Duration.inMilliseconds Animation.tileAnimationDuration)
        |> Task.map (always tilemapChange)
        |> Task.perform TilemapChanged



-- Views


overlay : World -> Maybe TilemapChange -> Tool -> Element Message
overlay world pendingTilemapChange tool =
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
                                , pendingTilemapChange = pendingTilemapChange
                                , cell = cell
                                }
                        , cell = cell
                        , pendingTilemapChange = pendingTilemapChange
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
    , pendingTilemapChange : Maybe TilemapChange
    , world : World
    , tool : Tool
    }
    -> Element Message
tileOverlay { glowColor, cell, pendingTilemapChange, world, tool } =
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
        , Events.onClick (choosePrimaryAction cell pendingTilemapChange tool world)
        , Element.htmlAttribute (CustomEvent.onRightClick <| chooseSecondaryAction cell pendingTilemapChange tool)
        ]
        Element.none


choosePrimaryAction : Cell -> Maybe TilemapChange -> Tool -> World -> Message
choosePrimaryAction cell pendingTilemapChange tool world =
    case ( tool, Tilemap.tileAt world.tilemap cell ) of
        ( SmartConstruction, _ ) ->
            let
                alreadyExists =
                    Tilemap.exists cell world.tilemap || cellChanging pendingTilemapChange cell
            in
            if not alreadyExists && Tilemap.canBuildRoadAt cell world.tilemap then
                AddTile cell

            else
                NoOp

        ( Bulldozer, Just _ ) ->
            if not (cellChanging pendingTilemapChange cell) then
                RemoveTile cell

            else
                NoOp

        ( Dynamite, _ ) ->
            ResetWorld

        _ ->
            NoOp


chooseSecondaryAction : Cell -> Maybe TilemapChange -> Tool -> Message
chooseSecondaryAction cell pendingTilemapChange tool =
    case tool of
        SmartConstruction ->
            if not (cellChanging pendingTilemapChange cell) then
                RemoveTile cell

            else
                NoOp

        _ ->
            NoOp


tileHighlight :
    { world : World
    , selectedTool : Tool
    , pendingTilemapChange : Maybe TilemapChange
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


cellChanging : Maybe TilemapChange -> Cell -> Bool
cellChanging pendingTilemapChange cell =
    case pendingTilemapChange of
        Just { origin } ->
            origin == cell

        Nothing ->
            False


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
