module PathfindingAndRouteBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.RuleSetups
import Simulation.Pathfinding as Pathfinding


suite : Benchmark
suite =
    describe "Pathfinding"
        [ describe "validateRoute"
            [ benchmark "routed" <|
                let
                    largeWorldSetup =
                        Data.RuleSetups.largeWorldSetup 1

                    car =
                        largeWorldSetup.activeCar
                in
                \_ -> Pathfinding.validateRoute largeWorldSetup.world car.route
            ]
        ]


main : BenchmarkProgram
main =
    program suite
