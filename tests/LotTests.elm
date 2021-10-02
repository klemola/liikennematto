module LotTests exposing (suite)

import Expect
import Model.Lot as Lot exposing (Lot)
import Model.Tilemap as Tilemap exposing (OrthogonalDirection(..), tileSize)
import Quantity
import Test exposing (Test, describe, test)


oneByOneLot : Maybe Lot
oneByOneLot =
    let
        newLot =
            { content =
                { kind = Lot.ResidentialA
                , entryDirection = Down
                }
            , width = tileSize
            , height = tileSize
            }
    in
    Tilemap.cellFromCoordinates ( 1, 2 )
        |> Maybe.andThen (Lot.createAnchor newLot)
        |> Maybe.map (Lot.build newLot)


twoByTwoLot : Maybe Lot
twoByTwoLot =
    let
        newLot =
            { content =
                { kind = Lot.ResidentialE
                , entryDirection = Down
                }
            , width = tileSize |> Quantity.multiplyBy 2
            , height = tileSize |> Quantity.multiplyBy 2
            }
    in
    Tilemap.cellFromCoordinates ( 1, 3 )
        |> Maybe.andThen (Lot.createAnchor newLot)
        |> Maybe.map (Lot.build newLot)


suite : Test
suite =
    describe "Lot"
        [ describe ".inBounds"
            [ test "validates if a cell is in lot's bounds for 1x1 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 1, 1 )) oneByOneLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test "validates if a cell is in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 1, 2 )) twoByTwoLot
                        |> Maybe.withDefault False
                        |> Expect.true "Expected the cell to be in the lot's bounds"
                )
            , test
                "validates if a cell is *NOT* in lot's bounds for 1x1 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 1, 2 )) oneByOneLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            , test "validates if a cell is *NOT* in lot's bounds for 2x2 lot"
                (\_ ->
                    Maybe.map2 Lot.inBounds (Tilemap.cellFromCoordinates ( 2, 3 )) twoByTwoLot
                        |> Maybe.withDefault True
                        |> Expect.false "Expected the cell to be out of the lot's bounds"
                )
            ]
        ]
