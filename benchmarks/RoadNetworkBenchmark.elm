module RoadNetworkBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Model.Tilemap as Tilemap
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
                world.tilemap
                    |> Tilemap.toList Tuple.pair
                    |> List.head
                    |> Maybe.map (\( cell, _ ) -> Infrastructure.removeRoadAt cell world)

        -- this benchmark is, at the moment, really heavy - something like 163 runs per sec. May crash yer browser!
        , benchmark "large map" <|
            \_ ->
                let
                    world =
                        Worlds.largeWorld
                in
                world.tilemap
                    |> Tilemap.toList Tuple.pair
                    |> List.head
                    |> Maybe.map (\( cell, _ ) -> Infrastructure.removeRoadAt cell world)
        ]


main : BenchmarkProgram
main =
    program suite
