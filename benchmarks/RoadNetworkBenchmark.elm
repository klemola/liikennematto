module RoadNetworkBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Simulation.Infrastructure as Infrastructure
import Worlds


suite : Benchmark
suite =
    describe "Road network graph performance"
        [ benchmark "small map" <|
            \_ ->
                let
                    world =
                        Worlds.worldWithFourWayIntersection
                in
                Infrastructure.createRoadNetwork world.tilemap world

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ ->
                let
                    world =
                        Worlds.largeWorld
                in
                Infrastructure.createRoadNetwork world.tilemap world
        ]


main : BenchmarkProgram
main =
    program suite
