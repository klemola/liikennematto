module RenderBenchmark exposing (main)

import Assets exposing (assets)
import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Cars exposing (carStyleToString, sedan)
import Data.Colors as Colors
import Data.RuleSetups
import Dict
import Lib.Collection
import Model.RenderCache as RenderCache
import Render
import Simulation.Car as Car


testCar =
    Car.build Nothing (Car.new (sedan Colors.red Colors.redDarker)) Lib.Collection.initialId


world =
    Data.RuleSetups.collisionSetupPathsIntersect.world


cache =
    RenderCache.new world


suite : Benchmark
suite =
    describe "Render"
        [ describe "asset lookup"
            [ benchmark "car" <|
                \_ ->
                    Dict.get (carStyleToString testCar.make.style) assets
            , benchmark "lot" <|
                \_ ->
                    Dict.get "LotFlowerShop" assets
            ]
        , describe "render asset"
            [ benchmark "car" <|
                \_ ->
                    Render.renderCar cache testCar.position testCar.orientation testCar.make
            , benchmark "car - lazy" <|
                \_ ->
                    Render.renderCarLazy cache testCar
            ]
        ]


main : BenchmarkProgram
main =
    program suite
