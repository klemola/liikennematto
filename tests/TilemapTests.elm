module TilemapTests exposing (suite)

import Expect
import Maybe.Extra as Maybe
import Model.Tilemap as Tilemap exposing (Tilemap)
import Test exposing (Test, describe, test)
import Utility exposing (tilemapFromCoordinates)


tilemapThatResemblesAIntersection : Tilemap
tilemapThatResemblesAIntersection =
    tilemapFromCoordinates
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 3, 1 )
        , ( 2, 2 )
        ]


tilemapThatResemblesACurve : Tilemap
tilemapThatResemblesACurve =
    tilemapFromCoordinates
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 1, 2 )
        ]


suite : Test
suite =
    describe "Tilemap"
        [ describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    Tilemap.cellFromCoordinates ( 2, 1 )
                        |> Maybe.map (Tilemap.tileAt tilemapThatResemblesAIntersection)
                        |> Maybe.map
                            (\tile ->
                                tile == Just Tilemap.intersectionTDown
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a T-shaped intersection after the mask is applied.")
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    Tilemap.cellFromCoordinates ( 1, 1 )
                        |> Maybe.map (Tilemap.tileAt tilemapThatResemblesACurve)
                        |> Maybe.map
                            (\tile ->
                                tile == Just Tilemap.curveTopLeft
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a curve road piece after the mask is applied.")
                )
            ]
        ]
