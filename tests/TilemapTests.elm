module TilemapTests exposing (suite)

import Expect
import Model.Tilemap as Tilemap
import Test exposing (Test, describe, test)
import Tilemaps
    exposing
        ( tilemapThatResemblesACurve
        , tilemapThatResemblesAIntersection
        )


suite : Test
suite =
    describe "Tilemap"
        [ describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    tilemapThatResemblesAIntersection
                        |> Tilemap.tileAt ( 2, 1 )
                        |> (\tile ->
                                tile == Just Tilemap.intersectionTDown
                           )
                        |> Expect.true "Expected a T-shaped intersection after the mask is applied."
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    tilemapThatResemblesACurve
                        |> Tilemap.tileAt ( 1, 1 )
                        |> (\tile ->
                                tile == Just Tilemap.curveTopLeft
                           )
                        |> Expect.true "Expected a curve road piece after the mask is applied."
                )
            ]
        ]
