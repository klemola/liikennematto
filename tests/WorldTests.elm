module WorldTests exposing (suite)

import Dict
import Expect
import Model.Lot as Lot exposing (Lot)
import Model.OrthogonalDirection exposing (OrthogonalDirection(..))
import Model.Tile exposing (tileSize)
import Model.Tilemap as Tilemap
import Model.World as World
import Quantity
import Test exposing (Test, describe, test)
import Utility exposing (createBoundingBox)
import Worlds
    exposing
        ( worldThatHasAVerticalRoadAtLeftSide
        , worldThatHasParallelRoads
        )


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
    Tilemap.cellFromCoordinates ( 1, 8 )
        |> Maybe.andThen (Lot.createAnchor newLot)
        |> Maybe.map (Lot.build newLot)


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
                        lots =
                            twoByTwoLot
                                |> Maybe.map (\lot -> Dict.fromList [ ( 1, lot ) ])
                                |> Maybe.withDefault Dict.empty

                        withLot =
                            worldThatHasAVerticalRoadAtLeftSide
                                |> (\world -> { world | lots = lots })
                    in
                    World.isEmptyArea (createBoundingBox ( 80, 160 ) 160 160) withLot
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "correctly determines if an area is empty (not enough space between roads)"
                (\_ ->
                    World.isEmptyArea (createBoundingBox ( 80, 160 ) 160 160) worldThatHasParallelRoads
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "reports area as filled if it's out of tilemap bounds"
                (\_ ->
                    World.isEmptyArea (createBoundingBox ( 720, 80 ) 240 160) worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            ]
        ]
