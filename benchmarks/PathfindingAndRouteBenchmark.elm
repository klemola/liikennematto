module PathfindingAndRouteBenchmark exposing (main)

import AStar
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.RuleSetups
import Data.Utility exposing (getStartAndEndNode)
import Simulation.Pathfinding as Pathfinding


suite : Benchmark
suite =
    describe "Routes and pathfinding"
        [ describe "Pathfinding"
            [ describe "validateRoute"
                [ benchmark "routed" <|
                    let
                        setup =
                            Data.RuleSetups.largeWorldSetup 1

                        car =
                            setup.activeCar
                    in
                    \_ -> Pathfinding.validateRoute setup.world car.route
                ]
            ]
        , describe "A*"
            [ benchmark "medium long route" <|
                let
                    world =
                        Data.RuleSetups.routeVisualizationSetup.world

                    nodePair =
                        getStartAndEndNode world 1 47
                in
                \_ ->
                    Maybe.map
                        (\( start, end ) ->
                            AStar.findPath start end world.roadNetwork
                        )
                        nodePair
            , benchmark "short route" <|
                let
                    world =
                        Data.RuleSetups.routeVisualizationSetup.world

                    nodePair =
                        getStartAndEndNode world 1 13
                in
                \_ ->
                    Maybe.map
                        (\( start, end ) ->
                            AStar.findPath start end world.roadNetwork
                        )
                        nodePair
            , benchmark "disconnected route" <|
                let
                    world =
                        Data.RuleSetups.routeVisualizationSetup.world

                    nodePair =
                        getStartAndEndNode world 1 44
                in
                \_ ->
                    Maybe.map
                        (\( start, end ) ->
                            AStar.findPath start end world.roadNetwork
                        )
                        nodePair
            ]
        ]


main : BenchmarkProgram
main =
    program suite
