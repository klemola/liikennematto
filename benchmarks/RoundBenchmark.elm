module RoundBenchmark exposing (main, suite)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Board
import Car
import Config
import Dict
import QuadTree
import Round exposing (Round)
import Rounds
import Set


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
        carBB =
            Car.boundingBox round.activeCar

        otherCarBBs =
            round.otherCars
                |> List.map
                    (\car ->
                        { id = car.id
                        , position = car.position
                        , boundingBox = Car.boundingBox car
                        }
                    )

        matchesSet =
            -- QuadTree -> choose neighbor cars -> Set of ids
            QuadTree.init Board.boundingBox 2
                |> QuadTree.insertList otherCarBBs
                |> QuadTree.neighborsWithin Config.tileSizeInMeters carBB
                |> List.foldl (\bounded acc -> Set.insert bounded.id acc) Set.empty
    in
    { round | otherCars = List.filter (\car -> Set.member car.id matchesSet) round.otherCars }


main : BenchmarkProgram
main =
    program suite
