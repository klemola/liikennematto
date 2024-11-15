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

import Common exposing (andCarry)
import Data.Assets exposing (Assets)
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
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig exposing (TileId)
import Tilemap.WFC as WFC
import UI.Core


type alias RenderCache msg =
    { pixelsToMetersRatio : PixelsToMetersRatio
    , tilemap : TilemapPresentation
    , tilemapDebug : TilemapPresentation
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
    , tilemapWidth : Length.Length
    , tilemapHeight : Length.Length
    , tileListFilter : TileListFilter
    , assets : Assets msg
    }


type alias TilemapPresentation =
    List StaticTilePresentation


type alias StaticTilePresentation =
    ( Cell, TileKind )


type alias DynamicTilesPresentation =
    List DynamicTilePresentation


type alias DynamicTilePresentation =
    ( Cell, TileId, Maybe Animation )


new : World -> Assets msg -> RenderCache msg
new { tilemap } assets =
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
    , tilemapDebug = toTilemapCache NoFilter tilemap
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    , tileListFilter = initialTileListFilter
    , assets = assets
    }


setPixelsToMetersRatio : UI.Core.ZoomLevel -> RenderCache msg -> RenderCache msg
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


setTileListFilter : TileListFilter -> RenderCache msg -> RenderCache msg
setTileListFilter tileListFilter cache =
    { cache | tileListFilter = tileListFilter }


setTilemapCache : Tilemap -> Maybe WFC.Model -> RenderCache msg -> RenderCache msg
setTilemapCache tilemap wfcModel cache =
    { cache
        | tilemap = toTilemapCache cache.tileListFilter tilemap

        -- TODO: the debug state should only be set if the debug layer is open
        -- it is costly
        , tilemapDebug =
            case wfcModel of
                Just wfc ->
                    toTilemapCache NoFilter (WFC.toTilemap wfc)

                Nothing ->
                    cache.tilemapDebug
    }


refreshTilemapCache : TilemapUpdateResult -> RenderCache msg -> ( RenderCache msg, DynamicTilesPresentation )
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
                    |> andCarry Tile.id
                    |> Maybe.map (\( tile, tileId ) -> ( cell, tileId, tileAnimation tile ))
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
