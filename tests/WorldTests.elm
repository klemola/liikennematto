module WorldTests exposing (suite)

import Dict
import Direction exposing (Direction(..))
import Expect
import Fixtures
import Test exposing (Test, describe, test)
import World


suite : Test
suite =
    describe "World.isEmptyArea"
        [ test "correctly determines if an area is empty (not existings lots)"
            (\_ ->
                World.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } Fixtures.worldWithEmptySpace
                    |> Expect.true "Expected the \"world\" to have space."
            )
        , test "correctly determines if an area is empty (existing lot in target area)"
            (\_ ->
                let
                    lot =
                        Fixtures.createTwoByTwoLot ( ( 1, 8 ), Right )

                    withLot =
                        Fixtures.worldWithEmptySpace
                            |> (\world -> { world | lots = Dict.fromList [ ( 1, lot ) ] })
                in
                World.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } withLot
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        , test "correctly determines if an area is empty (not enough space between roads)"
            (\_ ->
                World.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } Fixtures.worldWithParallelRoads
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        , test "reports area as filled if it's out of board bounds"
            (\_ ->
                World.isEmptyArea { origin = ( 720, 80 ), width = 240, height = 160 } Fixtures.worldWithEmptySpace
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        ]
