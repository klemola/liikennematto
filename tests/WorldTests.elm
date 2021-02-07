module WorldTests exposing (suite)

import Cell exposing (OrthogonalDirection(..))
import Dict
import Expect
import Fixtures
import Geometry
import Test exposing (Test, describe, test)
import World


suite : Test
suite =
    describe "World"
        [ describe "World.isEmptyArea"
            [ test "correctly determines if an area is empty (not existings lots)"
                (\_ ->
                    World.isEmptyArea { origin = Geometry.pointFromPosition ( 80, 160 ), width = 160, height = 160 } Fixtures.worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.true "Expected the \"world\" to have space."
                )
            , test "correctly determines if an area is empty (existing lot in target area)"
                (\_ ->
                    let
                        lot =
                            Fixtures.createTwoByTwoLot ( ( 1, 8 ), Right )

                        withLot =
                            Fixtures.worldThatHasAVerticalRoadAtLeftSide
                                |> (\world -> { world | lots = Dict.fromList [ ( 1, lot ) ] })
                    in
                    World.isEmptyArea { origin = Geometry.pointFromPosition ( 80, 160 ), width = 160, height = 160 } withLot
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "correctly determines if an area is empty (not enough space between roads)"
                (\_ ->
                    World.isEmptyArea { origin = Geometry.pointFromPosition ( 80, 160 ), width = 160, height = 160 } Fixtures.worldThatHasParallelRoads
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "reports area as filled if it's out of board bounds"
                (\_ ->
                    World.isEmptyArea { origin = Geometry.pointFromPosition ( 720, 80 ), width = 240, height = 160 } Fixtures.worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            ]
        , describe "World.canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    World.canBuildRoadAt ( 2, 2 ) Fixtures.lowComplexityWorld
                        |> Expect.true "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    World.canBuildRoadAt ( 2, 2 ) Fixtures.highComplexityWorld
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
