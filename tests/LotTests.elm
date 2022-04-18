module LotTests exposing (suite)

import Data.Lots
    exposing
        ( LotKind(..)
        , residentialSingle1
        , school
        )
import Data.Worlds exposing (lotTestWorld)
import Expect
import Model.Cell as Cell
import Model.Geometry exposing (OrthogonalDirection(..))
import Model.Lot as Lot exposing (Lot)
import Model.Tilemap as Tilemap
import Test exposing (Test, describe, test)


tilemapConfig =
    Tilemap.config lotTestWorld.tilemap


twoByTwoLot : Maybe Lot
twoByTwoLot =
    Cell.fromCoordinates tilemapConfig ( 4, 5 )
        |> Maybe.map (Lot.build tilemapConfig 1 residentialSingle1)


threeByThreeLot : Maybe Lot
threeByThreeLot =
    Cell.fromCoordinates tilemapConfig ( 1, 3 )
        |> Maybe.map (Lot.build tilemapConfig 2 school)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 (Lot.inBounds tilemapConfig)
                        (Cell.fromCoordinates tilemapConfig ( 2, 5 ))
                        twoByTwoLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 (Lot.inBounds tilemapConfig)
                        (Cell.fromCoordinates tilemapConfig ( 3, 1 ))
                        threeByThreeLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 (Lot.inBounds tilemapConfig)
                        (Cell.fromCoordinates tilemapConfig ( 2, 6 ))
                        twoByTwoLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 (Lot.inBounds tilemapConfig)
                        (Cell.fromCoordinates tilemapConfig ( 5, 2 ))
                        threeByThreeLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
