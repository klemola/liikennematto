module Model.RenderCache exposing
    ( DynamicTilePresentation
    , DynamicTilesPresentation
    , RenderCache
    , StaticTilePresentation
    , TilemapPresentation
    , new
    , refreshTilemapCache
    , setPixelsToMetersRatio
    , setTileListFilter
    , setTilemapCache
    )

import Data.Assets exposing (Assets)
import FSM
import Length
import Model.Animation as Animation exposing (Animation)
import Model.Geometry exposing (OrthogonalDirection, oppositeOrthogonalDirection)
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
        , tilemapToList
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig exposing (TileId)
import UI.Core


type alias RenderCache =
    { pixelsToMetersRatio : PixelsToMetersRatio
    , tilemap : TilemapPresentation
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
    , tilemapWidth : Length.Length
    , tilemapHeight : Length.Length
    , tileListFilter : TileListFilter
    , roadAssets : Assets ()
    }


type alias TilemapPresentation =
    List StaticTilePresentation


type alias StaticTilePresentation =
    ( Cell, TileKind )


type alias DynamicTilesPresentation =
    List DynamicTilePresentation


type alias DynamicTilePresentation =
    ( Cell, TileId, Maybe Animation )


new : World -> Assets () -> RenderCache
new { tilemap } roadAssets =
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
    , tilemap = toTilemapCache initialTileListFilter tilemap
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    , tileListFilter = initialTileListFilter
    , roadAssets = roadAssets
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


setTilemapCache : Tilemap -> RenderCache -> RenderCache
setTilemapCache tilemap cache =
    { cache | tilemap = toTilemapCache cache.tileListFilter tilemap }


setTileListFilter : TileListFilter -> RenderCache -> RenderCache
setTileListFilter tileListFilter cache =
    { cache | tileListFilter = tileListFilter }


refreshTilemapCache : TilemapUpdateResult -> RenderCache -> ( RenderCache, DynamicTilesPresentation )
refreshTilemapCache tilemapUpdateResult cache =
    let
        nextCache =
            if List.isEmpty tilemapUpdateResult.transitionedCells then
                cache

            else
                { cache | tilemap = toTilemapCache cache.tileListFilter tilemapUpdateResult.tilemap }

        nextDynamicTiles =
            if List.isEmpty tilemapUpdateResult.dynamicCells then
                []

            else
                toDynamicTiles
                    tilemapUpdateResult.tilemap
                    tilemapUpdateResult.dynamicCells
    in
    ( nextCache, nextDynamicTiles )


toTilemapCache : TileListFilter -> Tilemap -> List StaticTilePresentation
toTilemapCache tileListFilter tilemap =
    tilemapToList tileMapper tileListFilter tilemap


tileMapper : Cell -> Tile -> StaticTilePresentation
tileMapper cell tile =
    ( cell, tile.kind )


toDynamicTiles : Tilemap -> List Cell -> DynamicTilesPresentation
toDynamicTiles tilemap changingCells =
    changingCells
        |> List.filterMap
            (\cell ->
                fixedTileByCell tilemap cell
                    |> Maybe.andThen
                        (\tile ->
                            case tile.kind of
                                Fixed tileId ->
                                    Just ( cell, tileId, tileAnimation tile )

                                Superposition _ ->
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

        _ ->
            Nothing


animationDirectionFromTile : Tile -> Maybe OrthogonalDirection
animationDirectionFromTile tile =
    case Tile.potentialConnections tile of
        [ connection ] ->
            Just (oppositeOrthogonalDirection connection)

        _ ->
            Nothing
