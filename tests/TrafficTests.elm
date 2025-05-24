module TrafficTests exposing (suite)

import Acceleration exposing (Acceleration)
import Data.RuleSetups
    exposing
        ( collisionSetupCollided
        , collisionSetupNearCollision
        , collisionSetupPathsIntersect
        , connectedRoadsSetup
        , greenTrafficLightsSetup
        , noCollisionSetupDifferentLanes
        , noCollisionSetupIntersection
        , redTrafficLightsSetup
        , yieldSlowDownSetup
        , yieldWithPriorityTrafficSetup1
        , yieldWithPriorityTrafficSetup2
        , yieldWithoutPriorityTrafficSetup
        )
import Expect
import Quantity
import Simulation.Steering as Steering exposing (Steering)
import Simulation.Traffic as Traffic
import Test exposing (Test, describe, test)


getLinearAcceleration : Steering -> Acceleration
getLinearAcceleration steering =
    steering.linear |> Maybe.withDefault Quantity.zero


expectNegativeAcceleration : String -> Acceleration -> Expect.Expectation
expectNegativeAcceleration failOutput quantity =
    Quantity.lessThanZero quantity
        |> Expect.equal True
        |> Expect.onFail failOutput


expectPositiveAcceleration : String -> Acceleration -> Expect.Expectation
expectPositiveAcceleration failOutput quantity =
    Quantity.greaterThanOrEqualToZero quantity
        |> Expect.equal True
        |> Expect.onFail failOutput


suite : Test
suite =
    describe "Round"
        [ describe "Collision rules"
            [ test "allow movement if there will be no collision (straight road, different lanes)"
                (\_ -> Expect.equal (Traffic.checkRules noCollisionSetupDifferentLanes) Steering.accelerate)
            , test "allow movement if there will be no collision (intersection)"
                (\_ -> Expect.equal (Traffic.checkRules noCollisionSetupIntersection) Steering.accelerate)
            , test "disallow movement if it will cause a collision (paths intersect)"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected collision avoidance"
                        (collisionSetupPathsIntersect
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "disallow movement if it will cause a collision (near collision)"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected collision avoidance"
                        (collisionSetupNearCollision
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "recover when a collision occurs"
                (\_ -> Expect.equal (Traffic.checkRules collisionSetupCollided) Steering.reactToCollision)
            ]
        , describe "Intersection rules"
            [ test "allow movement if the car is not facing a intersection"
                (\_ -> Expect.equal (Traffic.checkRules connectedRoadsSetup) Steering.accelerate)
            , test "allow movement if traffic lights are green"
                (\_ ->
                    expectPositiveAcceleration
                        "Expected zero to positive acceleration"
                        (greenTrafficLightsSetup
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "allow movement if there's no need to yield at sign"
                (\_ -> Expect.equal (Traffic.checkRules yieldWithoutPriorityTrafficSetup) Steering.accelerate)
            ]
        , describe "Checking traffic rules"
            [ test "can prevent car movement"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected deceleration"
                        (collisionSetupNearCollision
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "can stop the car at traffic lights"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected deceleration"
                        (redTrafficLightsSetup
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "can make the car yield, setup 1"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected deceleration"
                        (yieldWithPriorityTrafficSetup1
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "can make the car yield, setup 2"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected deceleration"
                        (yieldWithPriorityTrafficSetup2
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            , test "can make the car slow down before a yield sign"
                (\_ ->
                    expectNegativeAcceleration
                        "Expected deceleration"
                        (yieldSlowDownSetup
                            |> Traffic.checkRules
                            |> getLinearAcceleration
                        )
                )
            ]
        ]
