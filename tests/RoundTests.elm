module RoundTests exposing (suite)

import Acceleration exposing (Acceleration)
import Car exposing (Status(..))
import Expect
import Fixtures exposing (..)
import Length
import Maybe.Extra
import Round
    exposing
        ( RoundResults
        , Rule(..)
        )
import Test exposing (..)


checkCollisionRules round =
    Maybe.Extra.orListLazy
        [ \() -> Round.checkForwardCollision round
        , \() -> Round.checkPathCollision round
        ]


checkIntersectionRules round =
    Maybe.Extra.orListLazy
        [ \() -> Round.checkTrafficLights round
        ]


suite : Test
suite =
    describe "Round"
        [ describe "Collision rules"
            [ test "allow movement if there will be no collision (straight road, different lanes)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupDifferentLanes) Nothing)
            , test "allow movement if there will be no collision (intersection)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupIntersection) Nothing)
            , test "disallow movement if it will cause a collision (paths intersect)"
                (\_ -> Expect.equal (Round.checkPathCollision collisionSetupPathsIntersect) (Just (AvoidCollision (Length.meters 4.8083261120684915))))
            , test "disallow movement if it will cause a collision (near collision)"
                (\_ -> Expect.equal (Round.checkForwardCollision collisionSetupNearCollision) (Just (AvoidCollision (Length.meters 4.313351365237916))))
            ]
        , describe "Intersection rules"
            [ test "allow movement if the car is not facing a intersection"
                (\_ -> Expect.equal (checkIntersectionRules connectedRoadsSetup) Nothing)
            , test "allow movement if traffic lights are green"
                (\_ -> Expect.equal (Round.checkTrafficLights greenTrafficLightsSetup) Nothing)
            , test "allow movement if there's no need to yield at sign"
                (\_ -> Expect.equal (Round.checkTrafficLights yieldWithoutPriorityTrafficSetup) Nothing)
            , test "disallow movement if traffic lights are not green"
                (\_ -> Expect.equal (Round.checkTrafficLights redTrafficLightsSetup) (Just (WaitForTrafficLights (Length.meters 8))))
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
                        Car.maxDeceleration
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
