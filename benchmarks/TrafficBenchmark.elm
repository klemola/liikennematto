module TrafficBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe, scale)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Collection
import Data.RuleSetups
    exposing
        ( collisionSetupNearCollision
        , collisionSetupPathsIntersect
        , largeWorldSetup
        , noCollisionSetupDifferentLanes
        , redTrafficLightsSetup
        )
import Model.World as World
import QuadTree
import Simulation.Traffic as Traffic exposing (RuleSetup)
import Tilemap.Cell as Cell


suite : Benchmark
suite =
    describe "Rules performance"
        [ describe "Rule setup fixtures in action"
            [ benchmark "collision: paths intersect" <|
                \_ -> Traffic.checkRules collisionSetupPathsIntersect
            , benchmark "collision: near collision" <|
                \_ -> Traffic.checkRules collisionSetupNearCollision
            , benchmark "collision: no collision" <|
                \_ -> Traffic.checkRules noCollisionSetupDifferentLanes
            , benchmark "traffic lights: red" <|
                \_ -> Traffic.checkRules redTrafficLightsSetup
            ]
        , describe "Large map"
            [ -- Due to random generation, the amount of cars in play might be smaller (can't place cars too close to another)
              [ 5, 50, 100 ]
                |> List.map largeWorldSetup
                |> List.map
                    (\ruleSetup ->
                        ( Collection.size ruleSetup.world.cars |> String.fromInt
                        , \_ -> Traffic.checkRules ruleSetup
                        )
                    )
                |> scale "cars amount - not optimized"
            , [ 5, 50, 100 ]
                |> List.map largeWorldSetup
                |> List.map chooseOtherCarsWithQuadtree
                |> List.map
                    (\ruleSetup ->
                        ( Collection.size ruleSetup.world.cars |> String.fromInt
                        , \_ -> Traffic.checkRules ruleSetup
                        )
                    )
                |> scale "cars amount - with quadtree"
            ]
        ]


chooseOtherCarsWithQuadtree : RuleSetup -> RuleSetup
chooseOtherCarsWithQuadtree ruleSetup =
    let
        nearbyCars =
            ruleSetup.world
                |> World.createLookup ruleSetup.otherCars
                |> QuadTree.neighborsWithin Cell.size ruleSetup.activeCar.boundingBox
    in
    { ruleSetup | otherCars = nearbyCars }


main : BenchmarkProgram
main =
    program suite
