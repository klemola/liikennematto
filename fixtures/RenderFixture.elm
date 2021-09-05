module RenderFixture exposing (main)

import Html exposing (Html)
import Render
import Rounds


main : Html msg
main =
    let
        world =
            Rounds.collisionSetupCollided.world
    in
    Render.view world { showRoadNetwork = False, showCarDebugVisuals = True }
