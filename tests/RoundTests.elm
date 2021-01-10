module RoundTests exposing (suite)

import Car exposing (Car, Status(..), TurnKind(..))
import Direction exposing (Direction(..))
import Expect
import Fixtures exposing (..)
import Round
    exposing
        ( Rule(..)
        , checkCollisionRules
        , checkIntersectionRules
        )
import Test exposing (..)


suite : Test
suite =
    describe "Round"
        [ describe "Collision rules"
            [ test "allow movement if there will be no collision"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetup) Nothing)
            , test "disallow movement if it will cause a collision"
                (\_ -> Expect.equal (checkCollisionRules collisionSetup) (Just AvoidCollision))
            ]
        , describe "Intersection rules"
            [ test "allow movement if the car is not facing a intersection"
                (\_ -> Expect.equal (checkIntersectionRules connectedRoadsSetup) Nothing)
            , test "allow movement if traffic lights are green"
                (\_ -> Expect.equal (checkIntersectionRules greenTrafficLightsSetup) Nothing)
            , test "allow movement if there's no need to yield at sign"
                (\_ -> Expect.equal (checkIntersectionRules yieldWithoutPriorityTrafficSetup) Nothing)
            , test "disallow movement if traffic lights are not green"
                (\_ -> Expect.equal (checkIntersectionRules redTrafficLightsSetup) (Just WaitForTrafficLights))
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
                        (collisionSetup
                            |> Round.play
                            |> getStatus
                        )
                        SkippingRound
                )
            , test "can make the car turn"
                (\_ ->
                    Expect.equal
                        (curveSetup
                            |> Round.play
                            |> getStatus
                        )
                        (Turning RightTurn)
                )
            , test "can stop the car at traffic lights"
                (\_ ->
                    Expect.equal
                        (redTrafficLightsSetup
                            |> Round.play
                            |> getStatus
                        )
                        WaitingForTrafficLights
                )
            , test "can make the car yield"
                (\_ ->
                    Expect.equal
                        (yieldWithPriorityTrafficSetup
                            |> Round.play
                            |> getStatus
                        )
                        Yielding
                )
            , test "can make the car stop (at sign)"
                (\_ ->
                    Expect.equal
                        (stopSetup
                            |> Round.play
                            |> getStatus
                        )
                        StoppedAtIntersection
                )
            ]
        ]


getStatus : ( Car, a ) -> Car.Status
getStatus ( car, _ ) =
    car.status
