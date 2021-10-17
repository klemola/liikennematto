module RenderFixture exposing (main)

import Html exposing (Html)
import Model.AnimationSchedule as AnimationSchedule
import Render
import Rounds


main : Html msg
main =
    let
        world =
            Rounds.collisionSetupCollided.world
    in
    Render.view world AnimationSchedule.empty { showRoadNetwork = False, showCarDebugVisuals = True }
