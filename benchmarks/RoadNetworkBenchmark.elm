module RoadNetworkBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Simulation.Infrastructure as Infrastructure
import Worlds


suite : Benchmark
suite =
    describe "Road network graph performance"
        [ benchmark "small map" <|
            \_ -> Infrastructure.removeRoadAt ( 1, 2 ) Worlds.worldWithFourWayIntersection

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ -> Infrastructure.removeRoadAt ( 1, 1 ) Worlds.largeWorld
        ]


main : BenchmarkProgram
main =
    program suite
