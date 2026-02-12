module RenderFixture exposing (main)

import Data.RuleSetups
import Element
import Helpers
import Html exposing (Html)
import Model.Debug
import Model.RenderCache as RenderCache
import Model.Screen as Screen
import Render
import Render.Debug
import Render.Viewport as Viewport


main : Html ()
main =
    let
        world =
            Data.RuleSetups.collisionSetupPathsIntersect.world

        cache =
            RenderCache.new world

        screenWidth =
            floor (Helpers.viewBoxToScreen cache.tilemapWidthPixels)

        screenHeight =
            floor (Helpers.viewBoxToScreen cache.tilemapHeightPixels)

        screen =
            Screen.fromDimensions screenWidth screenHeight

        viewport =
            Viewport.init
                { tilemapWidth = cache.tilemapWidthPixels
                , tilemapHeight = cache.tilemapHeightPixels
                , viewportWidth = cache.tilemapWidthPixels
                , viewportHeight = cache.tilemapHeightPixels
                }

        renderDebug =
            Render.Debug.view
                world
                cache
                (Model.Debug.initialDebugState
                    |> Model.Debug.toggleLayer Model.Debug.CarDebug
                    |> Model.Debug.toggleLayer Model.Debug.RoadNetworkDebug
                    |> Model.Debug.toggleLayer Model.Debug.WFCDebug
                )
                screen
                (Just viewport)
                |> Element.html
    in
    Render.view world cache screen (Just viewport)
        |> Element.html
        |> Element.el
            [ Element.width (Element.px screenWidth)
            , Element.height (Element.px screenHeight)
            , Element.inFront renderDebug
            ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
