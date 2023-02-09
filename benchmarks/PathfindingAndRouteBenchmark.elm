module PathfindingAndRouteBenchmark exposing (main)

import AStar
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collection
import Data.RuleSetups
import Data.Utility exposing (getStartAndEndNode)
import Simulation.Pathfinding as Pathfinding


suite : Benchmark
suite =
    describe "Routes and pathfinding"
        [ describe "Pathfinding"
            [ describe "restoreRoute"
                [ benchmark "one car" <|
                    let
                        setup =
                            Data.RuleSetups.largeWorldSetup 5

                        car =
                            setup.activeCar
                    in
                    \_ -> Pathfinding.restoreRoute setup.world car
                , benchmark "multiple cars (n=5)" <|
                    let
                        setup =
                            Data.RuleSetups.largeWorldSetup 5
                    in
                    \_ ->
                        Collection.foldl
                            (\_ car _ -> Pathfinding.restoreRoute setup.world car)
                            setup.activeCar
                            setup.world.cars
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
