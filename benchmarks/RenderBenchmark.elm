module RenderBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Cars exposing (testCar)


constantValue =
    Data.Cars.carAsset Data.Cars.sedan


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
                        testCar.detailColor
            , benchmark "variable color, lazy" <|
                \_ ->
                    Data.Cars.sedanGraphicsLazy
                        testCar.bodyColor
                        testCar.detailColor
            ]
        ]


main : BenchmarkProgram
main =
    program suite
