module Model.RenderCache exposing
    ( RenderCache
    , TilePresentation
    , TilemapPresentation
    , new
    , refreshTilemapCache
    )

import FSM
import Length
import Model.Animation as Animation exposing (Animation)
import Model.Cell exposing (Cell)
import Model.Geometry exposing (OrthogonalDirection, oppositeOrthogonalDirection)
import Model.Tile as Tile exposing (Tile, TileKind)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World exposing (World)
import Render.Conversion exposing (toPixelsValue)


type alias RenderCache =
    { tilemap : TilemapPresentation
    , tilemapWidthPixels : Float
    , tilemapHeightPixels : Float
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
            toPixelsValue tilemapDimensions.width

        tilemapHeigthPixels =
            toPixelsValue tilemapDimensions.height
    in
    { tilemap = toTilemapCache tilemap
    , tilemapWidthPixels =
        tilemapWidthPixels
    , tilemapHeightPixels = tilemapHeigthPixels
    , tilemapHeight = tilemapDimensions.height
    }


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
