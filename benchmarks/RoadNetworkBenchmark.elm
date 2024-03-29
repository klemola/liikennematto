module RoadNetworkBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Worlds as Worlds
import Model.World exposing (createRoadNetwork)


suite : Benchmark
suite =
    describe "Road network graph performance"
        [ benchmark "small map" <|
            \_ ->
                let
                    world =
                        Worlds.worldWithFourWayIntersection
                in
                createRoadNetwork world.tilemap world

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ ->
                let
                    world =
                        Worlds.largeWorld
                in
                createRoadNetwork world.tilemap world
        ]


main : BenchmarkProgram
main =
    program suite
