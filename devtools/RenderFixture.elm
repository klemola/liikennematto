module RenderFixture exposing (main)

import Data.RuleSetups as RuleSetups
import Element
import Html exposing (Html)
import Model.RenderCache as RenderCache
import Render
import Render.Debug


main : Html msg
main =
    let
        world =
            RuleSetups.collisionSetupPathsIntersect.world

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
                { showRoadNetwork = False, showCarDebugVisuals = True }
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
