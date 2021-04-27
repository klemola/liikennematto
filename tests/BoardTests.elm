module BoardTests exposing (suite)

import Board
import Boards
    exposing
        ( boardThatResemblesACurve
        , boardThatResemblesAIntersection
        , curveTile
        , intersectionTile
        )
import Dict
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Board"
        [ describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    Board.applyMask boardThatResemblesAIntersection
                        |> Dict.get ( 2, 1 )
                        |> (\tile ->
                                tile == Just intersectionTile
                           )
                        |> Expect.true "Expected a T-shaped intersection after the mask is applied."
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    Board.applyMask boardThatResemblesACurve
                        |> Dict.get ( 1, 1 )
                        |> (\tile ->
                                tile == Just curveTile
                           )
                        |> Expect.true "Expected a curve road piece after the mask is applied."
                )
            ]
        ]
