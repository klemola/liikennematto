module Model.RenderCache exposing
    ( RenderCache
    , TilemapPresentation
    , new
    , refreshTilemapCache
    )

import Model.Animation as Animation exposing (Animation)
import Model.FSM as FSM
import Model.OrthogonalDirection exposing (OrthogonalDirection, oppositeOrthogonalDirection)
import Model.Tile as Tile exposing (Tile, TileKind)
import Model.Tilemap as Tilemap exposing (Cell, Tilemap)
import Model.World exposing (World)


type alias RenderCache =
    { tilemap : TilemapPresentation
    }


type alias TilemapPresentation =
    List TilePresentation


type alias TilePresentation =
    ( Cell, TileKind, Maybe Animation )


new : World -> RenderCache
new { tilemap } =
    { tilemap = toTilemapCache tilemap }


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
