module WorldTests exposing (suite)

import Dict
import Expect
import Lots exposing (createTwoByTwoLot)
import Model.Board exposing (OrthogonalDirection(..))
import Model.World as World
import Test exposing (Test, describe, test)
import Utility exposing (createBoundingBox)
import Worlds
    exposing
        ( highComplexityWorld
        , lowComplexityWorld
        , worldThatHasAVerticalRoadAtLeftSide
        , worldThatHasParallelRoads
        )


suite : Test
suite =
    describe "World"
        [ describe "World.isEmptyArea"
            [ test "correctly determines if an area is empty (not existings lots)"
                (\_ ->
                    World.isEmptyArea (createBoundingBox ( 80, 160 ) 160 160) worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.true "Expected the \"world\" to have space."
                )
            , test "correctly determines if an area is empty (existing lot in target area)"
                (\_ ->
                    let
                        lot =
                            createTwoByTwoLot ( ( 1, 8 ), Right )

                        withLot =
                            worldThatHasAVerticalRoadAtLeftSide
                                |> (\world -> { world | lots = Dict.fromList [ ( 1, lot ) ] })
                    in
                    World.isEmptyArea (createBoundingBox ( 80, 160 ) 160 160) withLot
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "correctly determines if an area is empty (not enough space between roads)"
                (\_ ->
                    World.isEmptyArea (createBoundingBox ( 80, 160 ) 160 160) worldThatHasParallelRoads
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "reports area as filled if it's out of board bounds"
                (\_ ->
                    World.isEmptyArea (createBoundingBox ( 720, 80 ) 240 160) worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            ]
        , describe "World.canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    World.canBuildRoadAt ( 2, 2 ) lowComplexityWorld
                        |> Expect.true "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    World.canBuildRoadAt ( 2, 2 ) highComplexityWorld
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
