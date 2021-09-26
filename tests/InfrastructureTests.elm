module InfrastructureTests exposing (..)

import Expect
import Simulation.Infrastructure as Infrastructure
import Test exposing (Test, describe, test)
import Worlds
    exposing
        ( highComplexityWorld
        , lowComplexityWorld
        )


suite : Test
suite =
    describe "Infrastructure"
        [ describe
            "Infrastructure.canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    Infrastructure.canBuildRoadAt ( 2, 2 ) lowComplexityWorld
                        |> Expect.true "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    Infrastructure.canBuildRoadAt ( 2, 2 ) highComplexityWorld
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
