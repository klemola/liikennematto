module RenderFixture exposing (main)

import Html exposing (Html)
import Render
import Rounds


main : Html msg
main =
    let
        fixture =
            Rounds.noCollisionSetupDifferentLanes

        _ =
            Debug.log "cars" fixture.world.cars
    in
    Render.view fixture.world { showRoadNetwork = False, showCarDebugVisuals = True }
