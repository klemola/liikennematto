module SplineBenchmark exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import CubicSpline2d
import Direction2d
import Length
import Point2d
import Polyline2d
import Splines


testSpline : CubicSpline2d.CubicSpline2d Length.Meters coordinates
testSpline =
    Splines.curveSpline
        Point2d.origin
        (Point2d.meters 10 10)
        Direction2d.positiveX
        0.5


testNdSpline : Result (Point2d.Point2d Length.Meters coordinates) (CubicSpline2d.Nondegenerate Length.Meters coordinates)
testNdSpline =
    CubicSpline2d.nondegenerate testSpline


suite : Benchmark
suite =
    describe "Spline length calculation"
        [ benchmark "Polyline based approximation, 16 samples" <|
            \_ ->
                let
                    segments =
                        CubicSpline2d.segments 16 testSpline
                in
                Polyline2d.length segments
        , [ 0.5, 0.1, 0.01 ]
            |> List.map
                (\maxError ->
                    ( String.fromFloat maxError
                    , \_ ->
                        case testNdSpline of
                            Ok ndSpline ->
                                let
                                    a =
                                        CubicSpline2d.arcLengthParameterized { maxError = Length.meters maxError } ndSpline
                                in
                                CubicSpline2d.arcLength a

                            Err _ ->
                                Length.meters 42
                    )
                )
            |> scale "ArcLengthParameterized"
        ]


main : BenchmarkProgram
main =
    program suite
