module InfrastructureTests exposing (..)

import Expect
import Model.Tilemap as Tilemap
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
                    Tilemap.cellFromCoordinates ( 2, 2 )
                        |> Maybe.map (\cell -> Infrastructure.canBuildRoadAt cell lowComplexityWorld)
                        |> Maybe.withDefault False
                        |> Expect.true "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    Tilemap.cellFromCoordinates ( 2, 2 )
                        |> Maybe.map (\cell -> Infrastructure.canBuildRoadAt cell highComplexityWorld)
                        |> Maybe.withDefault False
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
