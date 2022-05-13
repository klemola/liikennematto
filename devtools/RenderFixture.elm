module RenderFixture exposing (main)

import Data.RuleSetups as RuleSetups
import Html exposing (Html)
import Model.RenderCache as RenderCache
import Render


main : Html msg
main =
    let
        world =
            RuleSetups.collisionSetupCollided.world
    in
    Render.view
        world
        (RenderCache.new world)
        { showRoadNetwork = False, showCarDebugVisuals = True }
