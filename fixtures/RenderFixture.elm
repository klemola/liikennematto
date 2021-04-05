module RenderFixture exposing (main)

import Fixtures
import Html exposing (Html)
import Render


main : Html msg
main =
    let
        fixture =
            Fixtures.collisionSetupPathsIntersect

        _ =
            Debug.log "cars" fixture.world.cars
    in
    Render.view fixture.world { showRoadNetwork = False, showCarDebugVisuals = True }
