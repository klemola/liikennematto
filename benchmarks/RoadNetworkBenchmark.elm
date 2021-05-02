module RoadNetworkBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import RoadNetwork
import Worlds


suite : Benchmark
suite =
    describe "Road network graph performance"
        [ benchmark "small map" <|
            \_ -> RoadNetwork.fromBoardAndLots Worlds.worldWithIntersection.board Worlds.worldWithIntersection.lots

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ -> RoadNetwork.fromBoardAndLots Worlds.largeWorld.board Worlds.largeWorld.lots
        ]


main : BenchmarkProgram
main =
    program suite
