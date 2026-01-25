module RenderFixture exposing (main)

import Data.RuleSetups
import Element
import Html exposing (Html)
import Model.Debug
import Model.RenderCache as RenderCache
import Model.Screen as Screen
import Render
import Render.Debug


main : Html ()
main =
    let
        world =
            Data.RuleSetups.collisionSetupPathsIntersect.world

        cache =
            RenderCache.new world

        renderWidth =
            floor cache.tilemapWidthPixels

        renderHeight =
            floor cache.tilemapHeightPixels

        -- For devtools, use tilemap size as screen (no scaling)
        screen =
            Screen.fromDimensions renderWidth renderHeight

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
                Nothing
                |> Element.html
    in
    Render.view world cache screen Nothing
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
