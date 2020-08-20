module BoardTests exposing (suite)

import Board
import Direction exposing (Direction(..), Orientation(..))
import Expect
import Fixtures
import Test exposing (Test, describe, test)
import Tile exposing (IntersectionControl(..), IntersectionShape(..), Tile(..))


suite : Test
suite =
    describe "Board"
        [ describe ".canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    Board.canBuildRoadAt ( 2, 2 ) Fixtures.lowComplexityBoard
                        |> Expect.true "Expected valid board."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    Board.canBuildRoadAt ( 2, 2 ) Fixtures.highComplexityBoard
                        |> Expect.false "Expected invalid board."
                )
            ]
        , describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    Board.applyMask Fixtures.boardThatResemblesAIntersection
                        |> Board.get ( 2, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.expectedIntersectionTile
                           )
                        |> Expect.true "Expected a T-shaped intersection after the mask is applied."
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    Board.applyMask Fixtures.boardThatResemblesACurve
                        |> Board.get ( 1, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.expectedCurveTile
                           )
                        |> Expect.true "Expected a curve road piece after the mask is applied."
                )
            , test "Retains traffic direction in tiles after the mask is applied"
                (\_ ->
                    Board.applyMask Fixtures.boardThatHasModifiersOnTiles
                        |> Board.get ( 1, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.expectedModifierTileA
                           )
                        |> Expect.true "Expected tile modifier to remain."
                )
            , test "Retains intersection control in tiles after the mask is applied"
                (\_ ->
                    Board.applyMask Fixtures.boardThatHasModifiersOnTiles
                        |> Board.get ( 3, 2 )
                        |> (\tile ->
                                tile == Just Fixtures.expectedModifierTileB
                           )
                        |> Expect.true "Expected tile modifier to remain."
                )
            ]
        ]
