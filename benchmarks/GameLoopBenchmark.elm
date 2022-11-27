module GameLoopBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.RuleSetups as RuleSetups
import Duration
import Message
import Model.Liikennematto as Liikennematto
import Simulation.Simulation as Simulation
import UI.Editor as Editor


baseModel =
    Liikennematto.new


suite : Benchmark
suite =
    describe "Game loop"
        [ describe "Tilemap update"
            [ benchmark "large world" <|
                let
                    setup =
                        RuleSetups.largeWorldSetup 1

                    msg =
                        Message.UpdateTilemap (Duration.milliseconds 16)

                    model =
                        { baseModel | world = setup.world }
                in
                \_ ->
                    Editor.update msg model
            ]
        , describe "Traffic update"
            [ benchmark "large world, cars n=1" <|
                let
                    setup =
                        RuleSetups.largeWorldSetup 1

                    msg =
                        Message.UpdateTraffic (Duration.milliseconds 16)

                    model =
                        { baseModel | world = setup.world }
                in
                \_ ->
                    Simulation.update msg model
            , benchmark "large world, cars n=50" <|
                let
                    setup =
                        RuleSetups.largeWorldSetup 50

                    msg =
                        Message.UpdateTraffic (Duration.milliseconds 16)

                    model =
                        { baseModel | world = setup.world }
                in
                \_ ->
                    Simulation.update msg model
            ]
        ]


main : BenchmarkProgram
main =
    program suite
