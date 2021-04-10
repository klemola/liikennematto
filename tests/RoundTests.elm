module RoundTests exposing (suite)

import Acceleration exposing (Acceleration)
import Car exposing (Status(..))
import Config
import Expect
import Fixtures exposing (..)
import Length
import Round
    exposing
        ( RoundResults
        , Rule(..)
        , checkCollisionRules
        , checkIntersectionRules
        )
import Test exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "Collision rules"
            [ test "allow movement if there will be no collision (straight road, different lanes)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupDifferentLanes) Nothing)
            , test "allow movement if there will be no collision (intersection)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupIntersection) Nothing)
            , test "disallow movement if it will cause a collision (paths intersect)"
                (\_ -> Expect.equal (checkCollisionRules collisionSetupPathsIntersect) (Just (AvoidCollision 2)))
            , test "disallow movement if it will cause a collision (near collision)"
                (\_ -> Expect.equal (checkCollisionRules collisionSetupNearCollision) (Just (PreventCollision 2)))
            ]
        , describe "Intersection rules"
            [ test "allow movement if the car is not facing a intersection"
                (\_ -> Expect.equal (checkIntersectionRules connectedRoadsSetup) Nothing)
            , test "allow movement if traffic lights are green"
                (\_ -> Expect.equal (checkIntersectionRules greenTrafficLightsSetup) Nothing)
            , test "allow movement if there's no need to yield at sign"
                (\_ -> Expect.equal (checkIntersectionRules yieldWithoutPriorityTrafficSetup) Nothing)
            , test "disallow movement if traffic lights are not green"
                (\_ -> Expect.equal (checkIntersectionRules redTrafficLightsSetup) (Just (WaitForTrafficLights (Length.meters 8))))
            , test "disallow movement if the car has to yield at sign"
                (\_ -> Expect.equal (checkIntersectionRules yieldWithPriorityTrafficSetup) (Just YieldAtIntersection))
            , test "disallow movement if the car is at a stop sign"
                (\_ -> Expect.equal (checkIntersectionRules stopSetup) (Just StopAtIntersection))
            , test "disallow movement if the car is at a stop sign - additional yield rules"
                (\_ -> Expect.equal (checkIntersectionRules yieldAfterStopSetup) (Just YieldAtIntersection))
            ]
        , describe "Playing the round"
            [ test "can prevent car movement"
                (\_ ->
                    Expect.equal
                        (collisionSetupNearCollision
                            |> Round.play
                            |> getCarAcceleration
                        )
                        Config.acceleration.breakingFast
                )
            , test "can stop the car at traffic lights"
                (\_ ->
                    Expect.equal
                        (redTrafficLightsSetup
                            |> Round.play
                            |> getCarStatus
                        )
                        WaitingForTrafficLights
                )
            , test "can make the car yield"
                (\_ ->
                    Expect.equal
                        (yieldWithPriorityTrafficSetup
                            |> Round.play
                            |> getCarStatus
                        )
                        Yielding
                )
            , test "can make the car stop (at sign)"
                (\_ ->
                    Expect.equal
                        (stopSetup
                            |> Round.play
                            |> getCarStatus
                        )
                        StoppedAtIntersection
                )
            ]
        ]


getCarStatus : RoundResults -> Car.Status
getCarStatus { car } =
    car.status


getCarAcceleration : RoundResults -> Acceleration
getCarAcceleration { car } =
    car.acceleration
