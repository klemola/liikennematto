module LotTests exposing (suite)

import Direction exposing (Direction(..))
import Expect
import Fixtures
import Lot
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 1x1 lot"
                (\_ ->
                    Lot.inBounds ( 1, 1 ) Fixtures.oneByOneLot
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Lot.inBounds ( 1, 2 ) Fixtures.twoByTwoLot
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 1x1 lot"
                (\_ ->
                    Lot.inBounds ( 1, 2 ) Fixtures.oneByOneLot
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Lot.inBounds ( 2, 3 ) Fixtures.twoByTwoLot
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
