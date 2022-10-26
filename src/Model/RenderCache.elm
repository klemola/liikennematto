module Model.RenderCache exposing
    ( RenderCache
    , TilePresentation
    , TilemapPresentation
    , new
    , refreshTilemapCache
    , setPixelsToMetersRatio
    )

import FSM
import Length
import Model.Animation as Animation exposing (Animation)
import Model.Cell exposing (Cell)
import Model.Editor as Editor
import Model.Geometry exposing (OrthogonalDirection, oppositeOrthogonalDirection)
import Model.Tile as Tile exposing (Tile, TileKind)
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
    List TilePresentation


type alias TilePresentation =
    ( Cell, TileKind, Maybe Animation )


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


refreshTilemapCache : Tilemap -> RenderCache -> RenderCache
refreshTilemapCache tilemap cache =
    { cache | tilemap = toTilemapCache tilemap }


toTilemapCache : Tilemap -> List TilePresentation
toTilemapCache tilemap =
    Tilemap.toList tileMapper tilemap


tileMapper : Cell -> Tile -> TilePresentation
tileMapper cell tile =
    ( cell, tile.kind, tileAnimation tile )


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
