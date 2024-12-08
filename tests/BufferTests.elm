module BufferTests exposing (suite)

import Data.Utility
    exposing
        ( cellsByTileKind
        , cellsByTileKindFromAscii
        , createCell
        , placeRoadAndUpdateBuffer
        , removeRoadAndUpdateBuffer
        , tenByTenTilemap
        , tilemapToAscii
        )
import Expect
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Test exposing (..)
import Tilemap.Cell as Cell
import Tilemap.Core
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
                        , ""
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
                            tileByCell tilemap (createCell constraints 3 6) |> Maybe.map .kind
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
        , describe ".removeBuffer"
            [ test "Should not generate buffer"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ) ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 5, 5 ) tilemap
                    in
                    Expect.all
                        [ \tileKinds -> Expect.equalLists tileKinds.superposition []
                        , \tileKinds -> Expect.equalLists tileKinds.fixed [ ( 6, 5 ) ]
                        ]
                        (cellsByTileKind tilemapAfterRemove)
                )
            , test "Should clear history"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 1, 1 ), ( 2, 1 ), ( 5, 5 ) ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 5, 5 ) tilemap
                    in
                    Expect.equal (List.length (getBuildHistory tilemapAfterRemove)) 0
                )
            , test "Should should remove buffer around the removed tile: deadend"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 3, 3 ), ( 4, 3 ), ( 5, 3 ), ( 6, 3 ) ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 6, 3 ) tilemap

                        expectedTilemap =
                            """
                            ---o------
                            ---o------
                            --xxx-----
                            ---o------
                            ---o------
                            ---o------
                            ----------
                            ----------
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            , test "Should remove buffer around the removed tile: middle of a straight road"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 3, 3 )
                                , ( 4, 3 )
                                , ( 5, 3 )
                                , ( 6, 3 )
                                , ( 7, 3 )

                                -- second road
                                , ( 3, 7 )
                                , ( 4, 7 )
                                , ( 5, 7 )
                                , ( 6, 7 )
                                , ( 7, 7 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 5, 3 ) tilemap

                        expectedTilemap =
                            """
                            ----------
                            ----------
                            --xx-xx---
                            ---ooo----
                            ---ooo----
                            ---ooo----
                            --xxxxx---
                            ---ooo----
                            ---ooo----
                            ---ooo----
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            , test "Should remove buffer around the removed tile: curve"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 3, 3 )
                                , ( 4, 3 )
                                , ( 5, 3 )
                                , ( 6, 3 )
                                , -- turn
                                  ( 6, 4 )
                                , ( 6, 5 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 6, 4 ) tilemap

                        expectedTilemap =
                            """
                            ---oo-----
                            ---oo-----
                            --xxxx----
                            ---oo-----
                            ---oox----
                            ---oo-----
                            ----------
                            ----------
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            , test "Should remove buffer around the removed tile: complex road network"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ -- main street
                                  ( 3, 3 )
                                , ( 4, 3 )
                                , ( 5, 3 )
                                , ( 6, 3 )
                                , ( 7, 3 )
                                , ( 8, 3 )

                                -- side 1
                                , ( 5, 4 )
                                , ( 5, 5 )
                                , ( 5, 6 )

                                -- side 2
                                , ( 8, 4 )
                                , ( 8, 5 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 8, 4 ) tilemap

                        expectedTilemap =
                            """
                            ---oooo---
                            ---oooo---
                            --xxxxxx--
                            -oooxoo---
                            -oooxoox--
                            ---oxoo---
                            ----------
                            ----------
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            , test "Should remove buffer around the removed tile: near the edge of the tilemap"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 10, 1 )
                                , ( 10, 2 )
                                , ( 10, 3 )
                                , ( 10, 4 )
                                , ( 10, 5 )
                                , ( 10, 6 )
                                , ( 10, 7 )
                                , ( 10, 8 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 10, 5 ) tilemap

                        expectedTilemap =
                            """
                            ---------x
                            ------ooox
                            ------ooox
                            ---------x
                            ----------
                            ---------x
                            ------ooox
                            ---------x
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            , test "Should remove buffer around the removed tile: a road loop"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 4, 4 )
                                , ( 5, 4 )
                                , ( 6, 4 )
                                , ( 6, 5 )
                                , ( 6, 6 )
                                , ( 5, 6 )
                                , ( 4, 6 )
                                , ( 4, 5 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 5, 6 ) tilemap

                        expectedTilemap =
                            """
                            ----o-----
                            ----o-----
                            ----o-----
                            ---xxx----
                            oooxoxooo-
                            ---x-x----
                            ----------
                            ----------
                            ----------
                            ----------
                            """
                    in
                    expectCellsMatch expectedTilemap tilemapAfterRemove
                )
            ]
        ]
