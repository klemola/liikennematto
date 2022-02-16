module LotTests exposing (suite)

import Expect
import Model.Geometry exposing (OrthogonalDirection(..))
import Model.Lot as Lot exposing (Lot)
import Model.Tile exposing (tileSize)
import Model.Tilemap as Tilemap
import Quantity
import Test exposing (Test, describe, test)


twoByTwoLot : Maybe Lot
twoByTwoLot =
    let
        newLot =
            { kind = Lot.ResidentialSingle1
            , drivewayExit = Down
            , width = tileSize |> Quantity.multiplyBy 2
            , height = tileSize |> Quantity.multiplyBy 2
            }
    in
    Tilemap.cellFromCoordinates ( 1, 3 )
        |> Maybe.andThen (Lot.createAnchor newLot)
        |> Maybe.map (Lot.build 1 newLot)


threeByThreeLot : Maybe Lot
threeByThreeLot =
    let
        newLot =
            { kind = Lot.School
            , drivewayExit = Down
            , width = tileSize |> Quantity.multiplyBy 3
            , height = tileSize |> Quantity.multiplyBy 3
            }
    in
    Tilemap.cellFromCoordinates ( 1, 2 )
        |> Maybe.andThen (Lot.createAnchor newLot)
        |> Maybe.map (Lot.build 2 newLot)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 1, 2 )) twoByTwoLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 3, 1 )) threeByThreeLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 2, 3 )) twoByTwoLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 3x3 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 4, 2 )) threeByThreeLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
