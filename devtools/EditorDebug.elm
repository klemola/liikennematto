module EditorDebug exposing (..)

import Data.Tiles exposing (allTiles, defaultTile)
import Editor.WFC
import Element
import Html exposing (Html)
import Model.Debug
import Model.RenderCache as RenderCache
import Model.Tile exposing (TileId, TileKind(..))
import Model.Tilemap exposing (TerrainType(..), TilemapConfig)
import Model.World as World
import Random
import Render
import Render.Debug


tilemapConfig : TilemapConfig
tilemapConfig =
    { verticalCellsAmount = 12
    , horizontalCellsAmount = 12
    , initialSeed = Random.initialSeed 666
    , defaultTile = defaultTile
    , tiles = allTiles
    }


main : Html msg
main =
    let
        world =
            World.empty tilemapConfig

        cache =
            RenderCache.new world

        renderWidth =
            floor cache.tilemapWidthPixels

        renderHeight =
            floor cache.tilemapHeightPixels

        renderDebug =
            Render.Debug.view
                world
                cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                )
                |> Element.html
    in
    Render.view world cache []
        |> Element.html
        |> Element.el
            [ Element.width (Element.px renderWidth)
            , Element.height (Element.px renderHeight)
            , Element.inFront renderDebug
            ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
