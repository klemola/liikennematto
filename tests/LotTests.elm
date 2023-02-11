module LotTests exposing (suite)

import Collection exposing (Id, initialId, nextId)
import Data.Lots
    exposing
        ( residentialSingle1
        , school
        )
import Data.Worlds exposing (lotTestWorld)
import Expect
import Model.Cell as Cell
import Model.Lot as Lot exposing (Lot)
import Model.Tilemap as Tilemap
import Test exposing (Test, describe, test)


tilemapConfig =
    Tilemap.config lotTestWorld.tilemap


id1 : Id
id1 =
    initialId


id2 : Id
id2 =
    nextId initialId


twoByTwoLot : Maybe Lot
twoByTwoLot =
    Cell.fromCoordinates tilemapConfig ( 4, 5 )
        |> Maybe.map (Lot.build residentialSingle1)
        |> Maybe.map (\builderFn -> builderFn id1)


threeByThreeLot : Maybe Lot
threeByThreeLot =
    Cell.fromCoordinates tilemapConfig ( 1, 3 )
        |> Maybe.map (Lot.build school)
        |> Maybe.map (\builderFn -> builderFn id2)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds
                        (Cell.fromCoordinates tilemapConfig ( 2, 5 ))
                        twoByTwoLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds
                        (Cell.fromCoordinates tilemapConfig ( 3, 1 ))
                        threeByThreeLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds
                        (Cell.fromCoordinates tilemapConfig ( 2, 6 ))
                        twoByTwoLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds
                        (Cell.fromCoordinates tilemapConfig ( 5, 2 ))
                        threeByThreeLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
