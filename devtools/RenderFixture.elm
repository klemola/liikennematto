module RenderFixture exposing (main)

import Data.Assets exposing (roads)
import Data.RuleSetups as RuleSetups
import Element
import Html exposing (Html)
import Model.Debug
import Model.RenderCache as RenderCache
import Render
import Render.Debug


main : Html ()
main =
    let
        world =
            RuleSetups.collisionSetupPathsIntersect.world

        cache =
            RenderCache.new world roads

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
