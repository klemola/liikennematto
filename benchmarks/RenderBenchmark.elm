module RenderBenchmark exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Color exposing (Color)
import Data.Cars
import Data.Colors
import Svg exposing (Svg)
import Svg.Lazy


sedanGraphicsWithConfigLazy : Color -> Svg msg
sedanGraphicsWithConfigLazy bodyColor =
    Svg.Lazy.lazy sedanGraphicsWithConfigProxy bodyColor


sedanGraphicsWithConfigProxy : Color -> Svg msg
sedanGraphicsWithConfigProxy bodyColor =
    Svg.g [] (Data.Cars.sedanGraphicsWithConfig bodyColor)


suite : Benchmark
suite =
    describe "Render"
        [ describe "Car asset"
            [ benchmark "constant" <|
                \_ ->
                    Data.Cars.carAsset Data.Cars.sedan
            , benchmark "variable color" <|
                \_ ->
                    Data.Cars.sedanGraphicsWithConfig Data.Colors.red
            , benchmark "variable color, lazy" <|
                \_ ->
                    sedanGraphicsWithConfigLazy Data.Colors.red
            ]
        ]


main : BenchmarkProgram
main =
    program suite
