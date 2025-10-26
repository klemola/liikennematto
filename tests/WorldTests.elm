module WorldTests exposing (suite)

import Data.Utility exposing (tenByTenTilemap)
import Expect
import Lib.SeedState as SeedState
import Model.World as World
import Random
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "World"
        [ describe "updateSeed"
            [ test "accumulates WFC step counts into World step count"
                (\_ ->
                    let
                        -- Create a world with some existing step count
                        initialWorld =
                            World.empty (Random.initialSeed 12345) tenByTenTilemap

                        -- Simulate World having already stepped the seed 5 times
                        worldWithSteps =
                            { initialWorld
                                | seedState =
                                    { initialSeed = 12345
                                    , currentSeed = initialWorld.seedState.currentSeed
                                    , stepCount = 5
                                    }
                            }

                        -- Simulate WFC returning a SeedState with 10 steps
                        wfcSeedState =
                            { initialSeed = 12345
                            , currentSeed = Random.initialSeed 67890
                            , stepCount = 10
                            }

                        -- Apply updateSeed
                        updatedWorld =
                            World.updateSeed wfcSeedState worldWithSteps
                    in
                    Expect.equal updatedWorld.seedState.stepCount 15
                        |> Expect.onFail "World step count should be 5 + 10 = 15"
                )
            ]
        ]
