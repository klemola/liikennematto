module BoardTests exposing (suite)

import Board
import Dict
import Direction exposing (Direction(..), Orientation(..))
import Expect
import Fixtures
import Test exposing (Test, describe, test)
import Tile exposing (IntersectionControl(..), IntersectionShape(..), Tile(..))


suite : Test
suite =
    describe "Board"
        [ describe ".applyMask"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    Board.applyMask Fixtures.boardThatResemblesAIntersection
                        |> Dict.get ( 2, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.intersectionTile
                           )
                        |> Expect.true "Expected a T-shaped intersection after the mask is applied."
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    Board.applyMask Fixtures.boardThatResemblesACurve
                        |> Dict.get ( 1, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.curveTile
                           )
                        |> Expect.true "Expected a curve road piece after the mask is applied."
                )
            , test "Retains traffic direction in tiles after the mask is applied"
                (\_ ->
                    Board.applyMask Fixtures.boardThatHasModifiersOnTiles
                        |> Dict.get ( 1, 1 )
                        |> (\tile ->
                                tile == Just Fixtures.modifierTileA
                           )
                        |> Expect.true "Expected tile modifier to remain."
                )
            , test "Retains intersection control in tiles after the mask is applied"
                (\_ ->
                    Board.applyMask Fixtures.boardThatHasModifiersOnTiles
                        |> Dict.get ( 3, 2 )
                        |> (\tile ->
                                tile == Just Fixtures.modifierTileB
                           )
                        |> Expect.true "Expected tile modifier to remain."
                )
            ]
        ]
