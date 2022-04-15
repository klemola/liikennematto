module LotTests exposing (suite)

import Data.Lots
    exposing
        ( LotKind(..)
        , residentialSingle1
        , school
        )
import Expect
import Model.Cell as Cell
import Model.Geometry exposing (OrthogonalDirection(..))
import Model.Lot as Lot exposing (Lot)
import Test exposing (Test, describe, test)


twoByTwoLot : Maybe Lot
twoByTwoLot =
    Cell.fromCoordinates ( 3, 2 )
        |> Maybe.map (Lot.build 1 residentialSingle1)


threeByThreeLot : Maybe Lot
threeByThreeLot =
    Cell.fromCoordinates ( 1, 3 )
        |> Maybe.map (Lot.build 2 school)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Cell.fromCoordinates ( 1, 2 )) twoByTwoLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Cell.fromCoordinates ( 3, 1 )) threeByThreeLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Cell.fromCoordinates ( 2, 3 )) twoByTwoLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Cell.fromCoordinates ( 5, 2 )) threeByThreeLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
