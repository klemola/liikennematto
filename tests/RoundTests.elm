module RoundTests exposing (suite)

import Acceleration exposing (Acceleration)
import Data.Rounds
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
import Length
import Maybe.Extra
import Quantity
import Simulation.Round as Round
    exposing
        ( Round
        , Rule(..)
        , checkTrafficControl
        )
import Simulation.Steering exposing (Steering)
import Test exposing (Test, describe, test)


checkCollisionRules round =
    Maybe.Extra.orListLazy
        [ \() -> Round.checkForwardCollision round
        , \() -> Round.checkPathCollision round
        ]


avoidingCollision : (Round -> Maybe Rule) -> Round -> Bool
avoidingCollision check round =
    case check round of
        Just (AvoidCollision _) ->
            True

        _ ->
            False


getLinearAcceleration : Steering -> Acceleration
getLinearAcceleration steering =
    steering.linear |> Maybe.withDefault Quantity.zero


suite : Test
suite =
    describe "Round"
        [ describe "Collision rules"
            [ test "allow movement if there will be no collision (straight road, different lanes)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupDifferentLanes) Nothing)
            , test "allow movement if there will be no collision (intersection)"
                (\_ -> Expect.equal (checkCollisionRules noCollisionSetupIntersection) Nothing)
            , test "disallow movement if it will cause a collision (paths intersect)"
                (\_ ->
                    Expect.true
                        "Expected collision avoidance"
                        (avoidingCollision Round.checkPathCollision collisionSetupPathsIntersect)
                )
            , test "disallow movement if it will cause a collision (near collision)"
                (\_ ->
                    Expect.true
                        "Expected collision avoidance"
                        (avoidingCollision Round.checkForwardCollision collisionSetupNearCollision)
                )
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
        , describe "Checking round rules"
            [ test "can prevent car movement"
                (\_ ->
                    Expect.true
                        "Expected deceleration"
                        (collisionSetupNearCollision
                            |> Round.checkRules
                            |> getLinearAcceleration
                            |> Quantity.lessThanZero
                        )
                )
            , test "can stop the car at traffic lights"
                (\_ ->
                    Expect.true
                        "Expected deceleration"
                        (redTrafficLightsSetup
                            |> Round.checkRules
                            |> getLinearAcceleration
                            |> Quantity.lessThanZero
                        )
                )
            , test "can make the car yield"
                (\_ ->
                    Expect.true
                        "Expected deceleration"
                        (yieldWithPriorityTrafficSetup1
                            |> Round.checkRules
                            |> getLinearAcceleration
                            |> Quantity.lessThanZero
                        )
                )
            ]
        ]
