module RoundBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Dict
import Round
import Rounds


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
              [ 5, 25, 50, 100 ]
                |> List.map Rounds.largeWorldSetup
                |> List.map
                    (\round ->
                        ( Dict.size round.world.cars |> String.fromInt
                        , \_ -> Round.play round
                        )
                    )
                |> scale "cars amount - not optimized"
            ]
        ]


main : BenchmarkProgram
main =
    program suite
