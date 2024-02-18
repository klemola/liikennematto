module CellTests exposing (..)

import Expect
import Model.Cell as Cell exposing (Boundary(..))
import Model.Geometry exposing (DiagonalDirection(..), OrthogonalDirection(..))
import Test exposing (Test, describe, test)


constraints =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


suite : Test
suite =
    describe "Cell"
        [ describe "connectedBounds"
            [ test "topLeftCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 1 )))
                        (Just (Corner TopLeft))
                )
            , test
                "topRightCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 1 )))
                        (Just (Corner TopRight))
                )
            , test
                "bottomLeftCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 10 )))
                        (Just (Corner BottomLeft))
                )
            , test
                "bottomRightCorner"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 10 )))
                        (Just (Corner BottomRight))
                )
            , test
                "leftEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 1, 2 )))
                        (Just (Edge Left))
                )
            , test
                "rightEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 10, 2 )))
                        (Just (Edge Right))
                )
            , test
                "topEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 5, 1 )))
                        (Just (Edge Up))
                )
            , test
                "bottomEdge"
                (\_ ->
                    Expect.equal
                        (Cell.connectedBounds constraints (Cell.fromCoordinatesUnsafe constraints ( 5, 10 )))
                        (Just (Edge Down))
                )
            ]
        ]
