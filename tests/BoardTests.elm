module BoardTests exposing (suite)

import Boards
    exposing
        ( boardThatResemblesACurve
        , boardThatResemblesAIntersection
        )
import Expect
import Model.Board as Board
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Board"
        [ describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    boardThatResemblesAIntersection
                        |> Board.tileAt ( 2, 1 )
                        |> (\tile ->
                                tile == Just Board.intersectionTDown
                           )
                        |> Expect.true "Expected a T-shaped intersection after the mask is applied."
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    boardThatResemblesACurve
                        |> Board.tileAt ( 1, 1 )
                        |> (\tile ->
                                tile == Just Board.curveTopLeft
                           )
                        |> Expect.true "Expected a curve road piece after the mask is applied."
                )
            ]
        ]
