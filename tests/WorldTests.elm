module WorldTests exposing (suite)

import Data.Utility exposing (tenByTenTilemap)
import Expect
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
        , describe "seed generation"
            [ test "stepping world seed produces different values"
                (\_ ->
                    let
                        world =
                            World.empty (Random.initialSeed 12345) tenByTenTilemap

                        -- Step the seed to generate a random int (simulating new game seed generation)
                        ( randomInt1, seed1 ) =
                            Random.step (Random.int Random.minInt Random.maxInt) (World.currentSeed world)

                        ( randomInt2, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) seed1

                        -- Values should be different
                        valuesDifferent =
                            randomInt1 /= randomInt2
                    in
                    Expect.equal valuesDifferent True
                        |> Expect.onFail "Successive random int generation should produce different values"
                )
            , test "new game seeds are deterministic from same starting seed"
                (\_ ->
                    let
                        world1 =
                            World.empty (Random.initialSeed 12345) tenByTenTilemap

                        world2 =
                            World.empty (Random.initialSeed 12345) tenByTenTilemap

                        ( randomInt1, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) (World.currentSeed world1)

                        ( randomInt2, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) (World.currentSeed world2)
                    in
                    Expect.equal randomInt1 randomInt2
                        |> Expect.onFail "Same starting seed should produce same random int"
                )
            ]
        ]
