module SharedStateTests exposing (suite)

import Dict
import Direction exposing (Direction(..))
import Expect
import Fixtures
import SharedState
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SharedState.isEmptyArea"
        [ test "correctly determines if an area is empty (not existings lots)"
            (\_ ->
                SharedState.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } Fixtures.sharedStateWithEmptySpace
                    |> Expect.true "Expected the \"world\" to have space."
            )
        , test "correctly determines if an area is empty (existing lot in target area)"
            (\_ ->
                let
                    lot =
                        Fixtures.createTwoByTwoLot ( ( 1, 8 ), Right )

                    withLot =
                        Fixtures.sharedStateWithEmptySpace
                            |> (\ss -> { ss | lots = Dict.fromList [ ( 1, lot ) ] })
                in
                SharedState.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } withLot
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        , test "correctly determines if an area is empty (not enough space between roads)"
            (\_ ->
                SharedState.isEmptyArea { origin = ( 80, 160 ), width = 160, height = 160 } Fixtures.sharedStateWithParallelRoads
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        , test "reports area as filled if it's out of board bounds"
            (\_ ->
                SharedState.isEmptyArea { origin = ( 720, 80 ), width = 240, height = 160 } Fixtures.sharedStateWithEmptySpace
                    |> Expect.false "Expected the \"world\" *not* to have space."
            )
        ]
