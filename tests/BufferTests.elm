module BufferTests exposing (suite)

import Data.Utility
    exposing
        ( addTileInstantly
        , cellsByTileKind
        , cellsByTileKindFromAscii
        , tenByTenTilemap
        , tilemapToAscii
        )
import Expect
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Test exposing (..)
import Tilemap.Buffer exposing (updateBufferCells)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core as Cell
    exposing
        ( Tilemap
        , createTilemap
        , getBuildHistory
        , getTilemapConfig
        , tileByCell
        )
import Tilemap.Tile as Tile


constraints : Cell.Constraints {}
constraints =
    tenByTenTilemap


createCell : Int -> Int -> Cell
createCell x y =
    Cell.fromCoordinatesUnsafe constraints ( x, y )


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap constraints (\_ -> Tile.init Tile.Unintialized)


expectCellsMatch : String -> Tilemap -> Expect.Expectation
expectCellsMatch expectedAsciiTilemap tilemap =
    let
        withoutLeadingWhitespace =
            expectedAsciiTilemap
                |> String.trim
                |> String.filter (\char -> char /= ' ')
    in
    case cellsByTileKindFromAscii (getTilemapConfig tilemap) withoutLeadingWhitespace of
        Ok cellsByTileKind_ ->
            Expect.equal (cellsByTileKind tilemap) cellsByTileKind_
                |> Expect.onFail
                    (String.join "\n"
                        [ multilineGridDebug "Expected..." withoutLeadingWhitespace
                        , multilineGridDebug "Actual  ..." (tilemapToAscii tilemap)
                        ]
                    )

        Err parseMessage ->
            Expect.fail parseMessage


multilineGridDebug : String -> String -> String
multilineGridDebug label str =
    str
        |> String.lines
        |> List.indexedMap
            (\i line ->
                let
                    lineNumber =
                        String.fromInt (i + 1) |> String.pad 2 ' '
                in
                String.join " " [ label, "line", lineNumber, line ]
            )
        |> String.join "\n"


placeRoadAndUpdateBuffer : List CellCoordinates -> Tilemap -> Tilemap
placeRoadAndUpdateBuffer cellsToPlace tilemap =
    List.foldl
        (\( x, y ) nextTilemap ->
            let
                cell =
                    createCell x y
            in
            nextTilemap
                |> addTileInstantly cell
                |> updateBufferCells cell
        )
        tilemap
        cellsToPlace



-- Test suite


suite : Test
suite =
    describe "Tilemap.Buffer"
        [ describe ".updateBufferCells"
            [ test "First two road tiles should not generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ) ]
                                emptyTilemap
                    in
                    Expect.all
                        [ \tileKinds -> Expect.equalLists tileKinds.superposition []
                        , \tileKinds -> Expect.equalLists tileKinds.fixed [ ( 5, 5 ), ( 6, 5 ) ]
                        ]
                        (cellsByTileKind tilemap)
                )
            , test "Non-adjacent road placement should clear history"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 1, 1 ), ( 2, 1 ), ( 5, 5 ) ]
                                emptyTilemap
                    in
                    Expect.equal (List.length (getBuildHistory tilemap)) 1
                )
            , test "Placing a single road tile with no neighbors should not generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ) ]
                                emptyTilemap
                    in
                    Expect.all
                        [ \_ ->
                            Expect.all
                                [ \tileKinds -> Expect.equalLists tileKinds.superposition []
                                , \tileKinds -> Expect.equalLists tileKinds.fixed [ ( 5, 5 ) ]
                                ]
                                (cellsByTileKind tilemap)
                        , \_ -> Expect.equal (List.length (getBuildHistory tilemap)) 1
                        ]
                        ()
                )
            , test "Buffer should not be generated in front of building direction"
                (\_ ->
                    let
                        tilemap =
                            -- Build top to bottom
                            placeRoadAndUpdateBuffer
                                [ ( 3, 3 ), ( 3, 4 ), ( 3, 5 ) ]
                                emptyTilemap

                        forwardTileKind =
                            tileByCell tilemap (createCell 3 6) |> Maybe.map .kind
                    in
                    Expect.equal forwardTileKind (Just Tile.Unintialized)
                )
            , test "Third road tile should generate buffer"
                (\_ ->
                    let
                        tilemap =
                            -- Build left to right
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                            ----------
                            -----o----
                            -----o----
                            -----o----
                            ----xxx---
                            -----o----
                            -----o----
                            -----o----
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test "Buffer should be generated behind and to the sides, within grid bounds"
                (\_ ->
                    let
                        tilemap =
                            -- Build right to left
                            placeRoadAndUpdateBuffer
                                [ ( 6, 3 ), ( 5, 3 ), ( 4, 3 ), ( 3, 3 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                            ---oo-----
                            ---oo-----
                            --xxxx----
                            ---oo-----
                            ---oo-----
                            ---oo-----
                            ----------
                            ----------
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test "Dead end should not generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                            ----------
                            -----oo---
                            -----oo---
                            -----oo---
                            ----xxxx--
                            -----oo---
                            -----oo---
                            -----oo---
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test "Joining two disconnected road pieces to form a straight road of 3 tiles should generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 4, 4 ), ( 4, 6 ), ( 4, 5 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                                ----------
                                ----------
                                ----------
                                ---x------
                                oooxooo---
                                ---x------
                                ----------
                                ----------
                                ----------
                                ----------
                                """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test "Placing roads out of order to form a straight road should generate buffer"
                (\_ ->
                    let
                        tilemap =
                            -- Build a set of two, disconnect, two, disconnect
                            placeRoadAndUpdateBuffer
                                [ ( 5, 6 ), ( 5, 5 ), ( 5, 3 ), ( 5, 4 ), ( 5, 2 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                                ----------
                                ----x-----
                                -oooxooo--
                                -oooxooo--
                                -oooxooo--
                                ----x-----
                                ----------
                                ----------
                                ----------
                                ----------
                                """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test "Joining two disconnected road pieces that don't form a straight line should not generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 2, 2 ), ( 3, 3 ), ( 3, 2 ) ]
                                emptyTilemap

                        expectedTilemap =
                            """
                                ----------
                                -xx-------
                                --x-------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                ----------
                                """
                    in
                    Expect.all
                        [ \_ -> expectCellsMatch expectedTilemap tilemap
                        , \_ -> Expect.equal (List.length (getBuildHistory tilemap)) 2
                        ]
                        ()
                )
            , test "Buffer should not appear around corners"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 2, 5 )
                                , ( 3, 5 )
                                , ( 4, 5 )
                                , ( 5, 5 )

                                -- Turn
                                , ( 6, 5 )
                                , ( 6, 4 )
                                , ( 6, 3 )
                                , ( 6, 2 )
                                ]
                                emptyTilemap

                        expectedTilemap =
                            """
                                ----------
                                --ooox----
                                --oooxooo-
                                --oooxooo-
                                -xxxxx----
                                --ooo-----
                                --ooo-----
                                --ooo-----
                                ----------
                                ----------
                                """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            , test
                "Buffer should not appear all around a T-intersection"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 2, 5 )
                                , ( 3, 5 )
                                , ( 4, 5 )
                                , ( 5, 5 )

                                -- To up
                                , ( 6, 5 )
                                , ( 6, 4 )
                                , ( 6, 3 )

                                -- To down
                                , ( 6, 6 )
                                , ( 6, 7 )
                                , ( 6, 8 )
                                ]
                                emptyTilemap

                        expectedTilemap =
                            """
                                ----------
                                --ooo-----
                                --ooox----
                                --oooxooo-
                                -xxxxx----
                                --oooxooo-
                                --oooxooo-
                                --ooox----
                                ----------
                                ----------
                                """
                    in
                    expectCellsMatch expectedTilemap tilemap
                )
            ]
        ]
