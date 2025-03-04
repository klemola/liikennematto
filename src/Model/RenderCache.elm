module Model.RenderCache exposing
    ( RenderCache
    , Renderable
    , TilemapDebugItem(..)
    , new
    , refreshTilemapCache
    , setPixelsToMetersRatio
    , setTileListFilter
    , setTilemapCache
    , setTilemapDebugCache
    )

import Data.TileSet exposing (roadConnectionDirectionsByTile)
import Length
import Lib.FSM as FSM
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Model.Animation as Animation exposing (Animation)
import Model.World exposing (World)
import Pixels
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio, defaultPixelsToMetersRatio, toPixelsValue)
import Tilemap.Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( TileListFilter(..)
        , Tilemap
        , TilemapUpdateResult
        , fixedTileByCell
        , getTilemapDimensions
        , tileToConfig
        , tilemapToList
        )
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig exposing (TileId)
import Tilemap.WFC as WFC
import UI.Core


type alias RenderCache =
    { pixelsToMetersRatio : PixelsToMetersRatio
    , tilemap : List Renderable
    , tilemapDebug : List ( Cell, TilemapDebugItem )
    , dynamicTiles : List Renderable
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
    , tilemapWidth : Length.Length
    , tilemapHeight : Length.Length
    , tileListFilter : TileListFilter
    }


type alias Renderable =
    { cell : Cell
    , assetName : String
    , animation : Maybe Animation
    }


type TilemapDebugItem
    = FixedDebug { id : TileId, parentTileId : Maybe TileId }
    | SuperpositionDebug (List TileId)


new : World -> RenderCache
new { tilemap } =
    let
        tilemapDimensions =
            getTilemapDimensions tilemap

        tilemapWidthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.width

        tilemapHeigthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.height

        initialTileListFilter =
            StaticTiles
    in
    { pixelsToMetersRatio = defaultPixelsToMetersRatio
    , tilemap = tilemapToList renderableFromTile initialTileListFilter tilemap
    , tilemapDebug = tilemapToList debugItemFromTile NoFilter tilemap
    , dynamicTiles = []
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    , tileListFilter = initialTileListFilter
    }


setPixelsToMetersRatio : UI.Core.ZoomLevel -> RenderCache -> RenderCache
setPixelsToMetersRatio zoomLevel cache =
    let
        nextPixelsPerMeter =
            zoomLevelToPixelsPerMeterValue zoomLevel

        nextRatio =
            Pixels.pixels nextPixelsPerMeter |> Quantity.per (Length.meters 1)
    in
    { cache
        | pixelsToMetersRatio = nextRatio
        , tilemapWidthPixels = toPixelsValue nextRatio cache.tilemapWidth
        , tilemapHeightPixels = toPixelsValue nextRatio cache.tilemapHeight
    }


zoomLevelToPixelsPerMeterValue : UI.Core.ZoomLevel -> Float
zoomLevelToPixelsPerMeterValue zoomLevel =
    case zoomLevel of
        UI.Core.VeryFar ->
            4

        UI.Core.Far ->
            6

        UI.Core.Near ->
            8


setTileListFilter : TileListFilter -> RenderCache -> RenderCache
setTileListFilter tileListFilter cache =
    { cache | tileListFilter = tileListFilter }


setTilemapCache : Tilemap -> Maybe Tilemap -> RenderCache -> RenderCache
setTilemapCache tilemap unsolvedWFCTilemap cache =
    { cache
        | tilemap = tilemapToList renderableFromTile cache.tileListFilter tilemap

        -- TODO: the debug state should only be set if the debug layer is open
        -- it is costly
        , tilemapDebug =
            case unsolvedWFCTilemap of
                Just wfcTilemap ->
                    tilemapToList debugItemFromTile NoFilter wfcTilemap

                Nothing ->
                    cache.tilemapDebug
    }


setTilemapDebugCache : WFC.Model -> RenderCache -> RenderCache
setTilemapDebugCache wfcModel cache =
    { cache | tilemapDebug = tilemapToList debugItemFromTile NoFilter (WFC.toTilemap wfcModel) }


refreshTilemapCache : TilemapUpdateResult -> RenderCache -> RenderCache
refreshTilemapCache tilemapUpdateResult cache =
    let
        nextCache =
            if List.isEmpty tilemapUpdateResult.transitionedCells then
                cache

            else
                setTilemapCache tilemapUpdateResult.tilemap Nothing cache

        nextDynamicTiles =
            if List.isEmpty tilemapUpdateResult.dynamicCells then
                []

            else
                toDynamicTiles
                    tilemapUpdateResult.tilemap
                    tilemapUpdateResult.dynamicCells
    in
    { nextCache | dynamicTiles = nextDynamicTiles }


renderableFromTile : Cell -> Tile -> Maybe Renderable
renderableFromTile cell tile =
    case tile.kind of
        Tile.Fixed props ->
            Just
                { cell = cell
                , assetName = props.name
                , animation = Nothing
                }

        _ ->
            Nothing


debugItemFromTile : Cell -> Tile -> Maybe ( Cell, TilemapDebugItem )
debugItemFromTile cell tile =
    case tile.kind of
        Tile.Fixed props ->
            Just
                ( cell
                , FixedDebug
                    { id = props.id
                    , parentTileId = Maybe.map Tuple.first props.parentTile
                    }
                )

        Tile.Superposition ids ->
            Just ( cell, SuperpositionDebug ids )

        _ ->
            Nothing


toDynamicTiles : Tilemap -> List Cell -> List Renderable
toDynamicTiles tilemap changingCells =
    changingCells
        |> List.filterMap
            (\cell ->
                fixedTileByCell tilemap cell
                    |> Maybe.andThen
                        (\tile ->
                            case tile.kind of
                                Tile.Fixed props ->
                                    Just
                                        { cell = cell
                                        , assetName = props.name
                                        , animation = tileAnimation tile
                                        }

                                _ ->
                                    Nothing
                        )
            )


tileAnimation : Tile -> Maybe Animation
tileAnimation tile =
    case FSM.toCurrentState tile.fsm of
        Tile.Constructing ->
            Just
                (Animation
                    Tile.transitionTimer
                    Animation.Appear
                    (animationDirectionFromTile tile)
                )

        Tile.Removing ->
            Just
                (Animation
                    Tile.transitionTimer
                    Animation.Disappear
                    Nothing
                )

        Tile.Generated ->
            -- TODO: consider animating these
            -- Remember to adjust Tile.isDynamic accordingly
            Nothing

        _ ->
            Nothing


animationDirectionFromTile : Tile -> Maybe OrthogonalDirection
animationDirectionFromTile tile =
    case
        tileToConfig tile
            |> Maybe.map roadConnectionDirectionsByTile
    of
        Just [ connection ] ->
            Just (OrthogonalDirection.opposite connection)

        _ ->
            Nothing
