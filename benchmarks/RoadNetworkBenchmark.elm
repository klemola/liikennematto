module RoadNetworkBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Worlds as Worlds
import RoadNetworkOptimized
import Simulation.RoadNetwork


suite : Benchmark
suite =
    describe "Road network performance: Original vs Optimized"
        [ describe "Small map (5 tiles)"
            [ benchmark "Original" <|
                \_ ->
                    let
                        world =
                            Worlds.worldWithFourWayIntersection
                    in
                    Simulation.RoadNetwork.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            , benchmark "Optimized" <|
                \_ ->
                    let
                        world =
                            Worlds.worldWithFourWayIntersection
                    in
                    RoadNetworkOptimized.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            ]
        , describe "Extra large map (50+ tiles)"
            [ benchmark "Original" <|
                \_ ->
                    let
                        world =
                            Worlds.largeWorld
                    in
                    Simulation.RoadNetwork.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            , benchmark "Optimized" <|
                \_ ->
                    let
                        world =
                            Worlds.largeWorld
                    in
                    RoadNetworkOptimized.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            ]
        ]


main : BenchmarkProgram
main =
    program suite
