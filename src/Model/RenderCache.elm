module Model.RenderCache exposing
    ( DynamicTilePresentation
    , DynamicTilesPresentation
    , RenderCache
    , StaticTilePresentation
    , TilemapPresentation
    , new
    , refreshTilemapCache
    , setPixelsToMetersRatio
    , setTilemapCache
    )

import FSM
import Length
import Model.Animation as Animation exposing (Animation)
import Model.Cell exposing (Cell)
import Model.Editor as Editor
import Model.Geometry exposing (OrthogonalDirection, oppositeOrthogonalDirection)
import Model.Tile as Tile exposing (Tile, TileKind(..))
import Model.TileConfig exposing (TileId)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World exposing (World)
import Pixels
import Quantity
import Render.Conversion exposing (PixelsToMetersRatio, defaultPixelsToMetersRatio, toPixelsValue)


type alias RenderCache =
    { pixelsToMetersRatio : PixelsToMetersRatio
    , tilemap : TilemapPresentation
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
    , tilemapWidth : Length.Length
    , tilemapHeight : Length.Length
    }


type alias TilemapPresentation =
    List StaticTilePresentation


type alias StaticTilePresentation =
    ( Cell, TileKind )


type alias DynamicTilesPresentation =
    List DynamicTilePresentation


type alias DynamicTilePresentation =
    ( Cell, TileId, Maybe Animation )


new : World -> RenderCache
new { tilemap } =
    let
        tilemapDimensions =
            Tilemap.dimensions tilemap

        tilemapWidthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.width

        tilemapHeigthPixels =
            toPixelsValue defaultPixelsToMetersRatio tilemapDimensions.height
    in
    { pixelsToMetersRatio = defaultPixelsToMetersRatio
    , tilemap = toTilemapCache tilemap
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapWidth = tilemapDimensions.width
    , tilemapHeight = tilemapDimensions.height
    }


setPixelsToMetersRatio : Editor.ZoomLevel -> RenderCache -> RenderCache
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


zoomLevelToPixelsPerMeterValue : Editor.ZoomLevel -> Float
zoomLevelToPixelsPerMeterValue zoomLevel =
    case zoomLevel of
        Editor.VeryFar ->
            4

        Editor.Far ->
            6

        Editor.Near ->
            8


setTilemapCache : Tilemap -> RenderCache -> RenderCache
setTilemapCache tilemap cache =
    { cache | tilemap = toTilemapCache tilemap }


refreshTilemapCache : Tilemap.TilemapUpdateResult -> RenderCache -> ( RenderCache, DynamicTilesPresentation )
refreshTilemapCache tilemapUpdateResult cache =
    let
        nextCache =
            if List.isEmpty tilemapUpdateResult.transitionedCells then
                cache

            else
                { cache | tilemap = toTilemapCache tilemapUpdateResult.tilemap }

        nextDynamicTiles =
            if List.isEmpty tilemapUpdateResult.dynamicCells then
                []

            else
                toDynamicTiles
                    tilemapUpdateResult.tilemap
                    tilemapUpdateResult.dynamicCells
    in
    ( nextCache, nextDynamicTiles )


toTilemapCache : Tilemap -> List StaticTilePresentation
toTilemapCache tilemap =
    Tilemap.toList tileMapper Tilemap.NoFilter tilemap


tileMapper : Cell -> Tile -> StaticTilePresentation
tileMapper cell tile =
    ( cell, tile.kind )


toDynamicTiles : Tilemap -> List Cell -> DynamicTilesPresentation
toDynamicTiles tilemap changingCells =
    changingCells
        |> List.filterMap
            (\cell ->
                Tilemap.tileAt tilemap cell
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
