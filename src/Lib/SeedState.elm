module Lib.SeedState exposing
    ( SeedState
    , currentSeed
    , fromInt
    , fromIntAndSteps
    , fromSeed
    , step
    , stepWithFunction
    )

import Random


type alias SeedState =
    { initialSeed : Int
    , currentSeed : Random.Seed
    , stepCount : Int
    }


fromSeed : Random.Seed -> SeedState
fromSeed seed =
    let
        ( initialSeedInt, _ ) =
            Random.step (Random.int Random.minInt Random.maxInt) seed
    in
    { initialSeed = initialSeedInt
    , currentSeed = seed
    , stepCount = 0
    }


fromInt : Int -> SeedState
fromInt initialSeedInt =
    { initialSeed = initialSeedInt
    , currentSeed = Random.initialSeed initialSeedInt
    , stepCount = 0
    }


fromIntAndSteps : Int -> Int -> SeedState
fromIntAndSteps initialSeedInt steps =
    let
        steppedSeed =
            stepNTimes steps (Random.initialSeed initialSeedInt)
    in
    { initialSeed = initialSeedInt
    , currentSeed = steppedSeed
    , stepCount = steps
    }


stepNTimes : Int -> Random.Seed -> Random.Seed
stepNTimes n seed =
    if n <= 0 then
        seed

    else
        let
            ( _, nextSeed ) =
                Random.step (Random.int Random.minInt Random.maxInt) seed
        in
        stepNTimes (n - 1) nextSeed


currentSeed : SeedState -> Random.Seed
currentSeed seedState =
    seedState.currentSeed


stepWithFunction : (Random.Seed -> ( a, Random.Seed )) -> SeedState -> ( a, SeedState )
stepWithFunction fn seedState =
    let
        ( value, nextSeed ) =
            fn seedState.currentSeed

        nextSeedState =
            { initialSeed = seedState.initialSeed
            , currentSeed = nextSeed
            , stepCount = seedState.stepCount + 1
            }
    in
    ( value, nextSeedState )


step : Random.Generator a -> SeedState -> ( a, SeedState )
step generator seedState =
    stepWithFunction (Random.step generator) seedState
