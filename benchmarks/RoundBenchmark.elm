module RoundBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import Model.Tilemap as Tilemap exposing (tileSize)
import QuadTree
import Rounds
import Simulation.Round as Round exposing (Round)


suite : Benchmark
suite =
    describe "Round performance"
        [ describe "Round fixtures in action"
            [ benchmark "collision: paths intersect" <|
                \_ -> Round.play Rounds.collisionSetupPathsIntersect
            , benchmark "collision: near collision" <|
                \_ -> Round.play Rounds.collisionSetupNearCollision
            , benchmark "collision: no collision" <|
                \_ -> Round.play Rounds.noCollisionSetupDifferentLanes
            , benchmark "traffic lights: red" <|
                \_ -> Round.play Rounds.redTrafficLightsSetup
            ]
        , describe "Large map"
            [ -- Due to random generation, the amount of cars in play might be smaller (can't place cars too close to another)
              [ 5, 50, 100 ]
                |> List.map Rounds.largeWorldSetup
                |> List.map
                    (\round ->
                        ( Dict.size round.world.cars |> String.fromInt
                        , \_ -> Round.play round
                        )
                    )
                |> scale "cars amount - not optimized"
            , [ 5, 50, 100 ]
                |> List.map Rounds.largeWorldSetup
                |> List.map chooseOtherCarsWithQuadtree
                |> List.map
                    (\round ->
                        ( Dict.size round.world.cars |> String.fromInt
                        , \_ -> Round.play round
                        )
                    )
                |> scale "cars amount - with quadtree"
            ]
        ]


chooseOtherCarsWithQuadtree : Round -> Round
chooseOtherCarsWithQuadtree round =
    let
        nearbyCars =
            QuadTree.init Tilemap.boundingBox 4
                |> QuadTree.insertList round.otherCars
                |> QuadTree.neighborsWithin tileSize round.activeCar.boundingBox
    in
    { round | otherCars = nearbyCars }


main : BenchmarkProgram
main =
    program suite
