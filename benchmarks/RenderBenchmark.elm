module RenderBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Cars exposing (testCar)
import Svg exposing (Svg)


constantValue : ( Svg msg, String )
constantValue =
    Data.Cars.carAsset Data.Cars.testCar


suite : Benchmark
suite =
    describe "Render"
        [ describe "Car asset"
            [ benchmark "constant" <|
                \_ ->
                    constantValue
            , benchmark "variable color" <|
                \_ ->
                    Data.Cars.sedanGraphics
                        testCar.bodyColor
                        testCar.accentColor
            , benchmark "variable color, lazy" <|
                \_ ->
                    Data.Cars.sedanGraphicsLazy
                        testCar.bodyColor
                        testCar.accentColor
            ]
        ]


main : BenchmarkProgram
main =
    program suite
