module SteeringTests exposing (suite)

import Acceleration
import Expect
import Length
import Quantity
import Simulation.Steering as Steering
import Speed
import Test exposing (Test, describe, test)


maxDecelerationRaw =
    Acceleration.inMetersPerSecondSquared Steering.maxDeceleration


suite : Test
suite =
    describe "Steering"
        [ describe "reachTargetVelocity"
            [ test "with positive velocity"
                (\_ ->
                    let
                        result =
                            Steering.reachTargetVelocity
                                (Speed.metersPerSecond 10)
                                (Speed.metersPerSecond 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.lessThan 0
                )
            , test "with negative velocity"
                (\_ ->
                    let
                        result =
                            Steering.reachTargetVelocity
                                (Speed.metersPerSecond -5)
                                (Speed.metersPerSecond 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.greaterThan 0
                )
            , test "edge case: with zero velocity"
                (\_ ->
                    let
                        result =
                            Steering.reachTargetVelocity
                                (Speed.metersPerSecond 0)
                                (Speed.metersPerSecond 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.greaterThan 0
                )
            , test "edge case: reach zero velocity"
                (\_ ->
                    let
                        result =
                            Steering.reachTargetVelocity
                                (Speed.metersPerSecond 5)
                                (Speed.metersPerSecond 0)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.lessThan 0
                )
            , test "edge case: near zero velocity"
                (\_ ->
                    let
                        result =
                            Steering.reachTargetVelocity
                                (Speed.metersPerSecond 0.05)
                                (Speed.metersPerSecond 0)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.within (Expect.Absolute 0.001) -0.1
                )
            ]
        , describe "accelerateToZeroOverDistance"
            [ test "over a long distance"
                (\_ ->
                    let
                        result =
                            Steering.accelerateToZeroOverDistance
                                (Speed.metersPerSecond 10)
                                (Length.meters 10)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.all
                            [ Expect.greaterThan maxDecelerationRaw
                            , Expect.lessThan 0
                            ]
                )
            , test "over a short distance"
                (\_ ->
                    let
                        result =
                            Steering.accelerateToZeroOverDistance
                                (Speed.metersPerSecond 10)
                                (Length.meters 2)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.equal maxDecelerationRaw
                )
            , test "with low positive velocity"
                (\_ ->
                    let
                        result =
                            Steering.accelerateToZeroOverDistance
                                (Speed.metersPerSecond 1)
                                (Length.meters 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.all
                            [ Expect.greaterThan maxDecelerationRaw
                            , Expect.lessThan 0
                            ]
                )
            , test "with negative velocity"
                (\_ ->
                    let
                        result =
                            Steering.accelerateToZeroOverDistance
                                (Speed.metersPerSecond -5)
                                (Length.meters 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.greaterThan 0
                )
            , test "edge case: with zero velocity"
                (\_ ->
                    let
                        result =
                            Steering.accelerateToZeroOverDistance
                                Quantity.zero
                                (Length.meters 5)
                    in
                    result
                        |> Acceleration.inMetersPerSecondSquared
                        |> Expect.greaterThan 0
                )
            ]
        ]
