module RenderFixture exposing (main)

import Html exposing (Html)
import Model.ActiveAnimations as ActiveAnimations
import Render
import Rounds


main : Html msg
main =
    let
        world =
            Rounds.collisionSetupCollided.world
    in
    Render.view world ActiveAnimations.empty { showRoadNetwork = False, showCarDebugVisuals = True }
