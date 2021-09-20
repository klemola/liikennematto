module RoadNetworkBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Simulation.Infrastructure as Infrastructure
import Worlds


suite : Benchmark
suite =
    describe "Road network graph performance"
        [ benchmark "small map" <|
            \_ -> Infrastructure.build Worlds.worldWithFourWayIntersection.board Worlds.worldWithFourWayIntersection.lots Worlds.worldWithFourWayIntersection.trafficLights

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ -> Infrastructure.build Worlds.largeWorld.board Worlds.largeWorld.lots Worlds.largeWorld.trafficLights
        ]


main : BenchmarkProgram
main =
    program suite
