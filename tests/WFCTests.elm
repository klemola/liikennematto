module WFCTests exposing (suite)

import Data.TileSet exposing (threeByThreeLotLargeTile, twoByTwoLotLargeTile)
import Data.Utility
    exposing
        ( createCell
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        )
import Expect
import Test exposing (..)
import Tilemap.Cell as Cell
import Tilemap.Core exposing (Tilemap, createTilemap, setSuperpositionOptions)
import Tilemap.Tile as Tile
import Tilemap.WFC as WFC


constraints : Cell.Constraints {}
constraints =
    tenByTenTilemap


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap constraints (\_ -> Tile.init Tile.Unintialized)


suite : Test
suite =
    describe "Tilemap.WFC"
        [ describe ".checkLargeTileFit"
            [ test "Should find fitting tiles (vertical road)"
                (\_ ->
                    let
                        twoByTwoLotDriveway =
                            createCell constraints 2 4

                        twoByTwoLotEntry =
                            createCell constraints 3 4

                        threeByThreeLotDriveway =
                            createCell constraints 4 5

                        threeByThreeLotEntry =
                            createCell constraints 3 5

                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 ), ( 3, 5 ), ( 3, 6 ) ]
                                emptyTilemap
                                |> setSuperpositionOptions twoByTwoLotEntry [ 22, 9 ]
                                |> setSuperpositionOptions threeByThreeLotEntry [ 21, 9 ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal (WFC.checkLargeTileFit tilemap twoByTwoLotDriveway twoByTwoLotLargeTile) (Just twoByTwoLotLargeTile)
                        , \_ -> Expect.equal (WFC.checkLargeTileFit tilemap threeByThreeLotDriveway threeByThreeLotLargeTile) (Just threeByThreeLotLargeTile)
                        ]
                        ()
                )
            ]
        ]
