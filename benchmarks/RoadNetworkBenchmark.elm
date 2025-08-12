module RoadNetworkBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Worlds as Worlds
import Simulation.RoadNetwork


suite : Benchmark
suite =
    describe "Road network performance"
        [ benchmark "Small map (5 tiles)" <|
            \_ ->
                let
                    world =
                        Worlds.worldWithFourWayIntersection
                in
                Simulation.RoadNetwork.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
        , benchmark "Extra large map (50+ tiles)" <|
            \_ ->
                let
                    world =
                        Worlds.largeWorld
                in
                Simulation.RoadNetwork.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
        ]


main : BenchmarkProgram
main =
    program suite
