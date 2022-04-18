module RenderFixture exposing (main)

import Data.Rounds as Rounds
import Html exposing (Html)
import Model.RenderCache as RenderCache
import Render


main : Html msg
main =
    let
        world =
            Rounds.collisionSetupCollided.world
    in
    Render.view
        world
        (RenderCache.new world)
        { showRoadNetwork = False, showCarDebugVisuals = True }
