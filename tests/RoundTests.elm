module RoundTests exposing (suite)

import Acceleration exposing (Acceleration)
import Expect
import Length
import Maybe.Extra
import Quantity
import Rounds
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
import Simulation.Round as Round
    exposing
        ( RoundResults
        , Rule(..)
        , checkTrafficControl
        )
import Simulation.Steering as Steering
import Test exposing (..)


checkCollisionRules round =
    Maybe.Extra.orListLazy
        [ \() -> Round.checkForwardCollision round
        , \() -> Round.checkPathCollision round
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
                (\_ -> Expect.equal (Round.checkPathCollision collisionSetupPathsIntersect) (Just (AvoidCollision (Length.meters 9.199999999999989))))
            , test "disallow movement if it will cause a collision (near collision)"
                (\_ -> Expect.equal (Round.checkForwardCollision collisionSetupNearCollision) (Just (AvoidCollision (Length.meters 4.313351365237916))))
            , test "recover when a collision occurs"
                (\_ -> Expect.equal (Round.checkForwardCollision collisionSetupCollided) (Just ReactToCollision))
            ]
        , describe "Intersection rules"
            [ test "allow movement if the car is not facing a intersection"
                (\_ -> Expect.equal (checkTrafficControl connectedRoadsSetup) Nothing)
            , test "allow movement if traffic lights are green"
                (\_ -> Expect.equal (checkTrafficControl greenTrafficLightsSetup) Nothing)
            , test "allow movement if there's no need to yield at sign"
                (\_ -> Expect.equal (checkTrafficControl yieldWithoutPriorityTrafficSetup) Nothing)
            , test "disallow movement if traffic lights are not green"
                (\_ -> Expect.equal (checkTrafficControl redTrafficLightsSetup) (Just (StopAtTrafficControl (Length.meters 8))))
            , test "disallow movement if the car has to yield at sign - 1"
                (\_ -> Expect.equal (checkTrafficControl yieldWithPriorityTrafficSetup1) (Just (StopAtTrafficControl (Length.meters 4))))
            , test "disallow movement if the car has to yield at sign - 2"
                (\_ -> Expect.equal (checkTrafficControl yieldWithPriorityTrafficSetup2) (Just (StopAtTrafficControl (Length.meters 4))))
            , test "slow down before a yield sign"
                (\_ -> Expect.equal (checkTrafficControl yieldSlowDownSetup) (Just SlowDownAtTrafficControl))
            ]
        , describe "Playing the round"
            [ test "can prevent car movement"
                (\_ ->
                    Expect.equal
                        (collisionSetupNearCollision
                            |> Round.play
                            |> getCarAcceleration
                        )
                        Steering.maxDeceleration
                )
            , test "can stop the car at traffic lights"
                (\_ ->
                    Expect.true
                        "Expected deceleration"
                        (redTrafficLightsSetup
                            |> Round.play
                            |> getCarAcceleration
                            |> Quantity.lessThanZero
                        )
                )
            , test "can make the car yield"
                (\_ ->
                    Expect.true
                        "Expected deceleration"
                        (yieldWithPriorityTrafficSetup1
                            |> Round.play
                            |> getCarAcceleration
                            |> Quantity.lessThanZero
                        )
                )
            ]
        ]


getCarAcceleration : RoundResults -> Acceleration
getCarAcceleration { car } =
    car.acceleration
