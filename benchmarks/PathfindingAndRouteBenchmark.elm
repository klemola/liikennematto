module PathfindingAndRouteBenchmark exposing (main)

import AStar
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.RuleSetups
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Model.World exposing (World)
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


getStartAndEndNode : World -> Int -> Int -> Maybe ( RNNodeContext, RNNodeContext )
getStartAndEndNode world startId endId =
    let
        start =
            RoadNetwork.findNodeByNodeId world.roadNetwork startId

        end =
            RoadNetwork.findNodeByNodeId world.roadNetwork endId
    in
    Maybe.map2 Tuple.pair start end


main : BenchmarkProgram
main =
    program suite
