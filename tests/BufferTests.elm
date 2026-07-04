module BufferTests exposing (suite)

import Data.TileSet exposing (defaultTileId, tileById, twoByTwoLotRightLargeTile)
import Data.Utility
    exposing
        ( cellsByTileKind
        , cellsByTileKindFromAscii
        , createCell
        , forceFixLargeNatureTile
        , forceFixNatureTile
        , multilineGridDebug
        , placeRoad
        , placeRoadAndUpdateBuffer
        , removeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        , tilemapToAscii
        )
import Dict
import Expect
import Lib.SeedState as SeedState
import Test exposing (Test, describe, test)
import Tilemap.Buffer exposing (reconcileSavedNatureTiles, revertSavedNature)
import Tilemap.Cell as Cell
import Tilemap.Core
    exposing
        ( Tilemap
        , createTilemap
        , getBuildHistory
        , getSavedNatureAnchors
        , getSavedNatureTiles
        , getTilemapConfig
        , insertSavedNatureAnchor
        , insertSavedNatureTile
        , mapCell
        , tileByCell
        )
import Tilemap.DrivenWFC
import Tilemap.Tile as Tile exposing (TileKind(..))
import Tilemap.TileConfig as TileConfig
import Tilemap.WFC


constraints : Cell.Constraints {}
constraints =
    tenByTenTilemap


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap constraints (\_ -> Tile.init Tile.Unintialized)


{-| NatureDouble1: a 1x2 Large Nature tile
-}
natureDouble1Id : TileConfig.TileId
natureDouble1Id =
    215


lotLargeTileId : TileConfig.TileId
lotLargeTileId =
    twoByTwoLotRightLargeTile.id


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
                            -----b----
                            -----b----
                            -----b----
                            ----xxx---
                            -----b----
                            -----b----
                            -----b----
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
                            ---bb-----
                            ---bb-----
                            --xxxx----
                            ---bb-----
                            ---bb-----
                            ---bb-----
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
                            -----bb---
                            -----bb---
                            -----bb---
                            ----xxxx--
                            -----bb---
                            -----bb---
                            -----bb---
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
                                bbbxbbb---
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
                                -bbbxbbb--
                                -bbbxbbb--
                                -bbbxbbb--
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
                        , \_ -> Expect.equal (List.length (getBuildHistory tilemap)) 1
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
                                --bbbx----
                                --bbbxbbb-
                                --bbbxbbb-
                                -xxxxx----
                                --bbb-----
                                --bbb-----
                                --bbb-----
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
                                --bbb-----
                                --bbbx----
                                --bbbxbbb-
                                -xxxxx----
                                --bbbxbbb-
                                --bbbxbbb-
                                --bbbx----
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
                            ---b------
                            ---b------
                            --xxx-----
                            ---b------
                            ---b------
                            ---b------
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
                            ---bbb----
                            ---bbb----
                            ---bbb----
                            --xxxxx---
                            ---bbb----
                            ---bbb----
                            ---bbb----
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
                            ---bb-----
                            ---bb-----
                            --xxxx----
                            ---bb-----
                            ---bbx----
                            ---bb-----
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
                            ---bbbb---
                            ---bbbb---
                            --xxxxxx--
                            -bbbxbb---
                            -bbbxbbx--
                            ---bxbb---
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
                            ------bbbx
                            ------bbbx
                            ---------x
                            ----------
                            ---------x
                            ------bbbx
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
                                , ( 4, 5 )
                                , ( 4, 6 )
                                , ( 6, 5 )
                                , ( 6, 6 )
                                , ( 5, 6 )
                                ]
                                emptyTilemap

                        tilemapAfterRemove =
                            removeRoadAndUpdateBuffer ( 5, 6 ) tilemap

                        expectedTilemap =
                            """
                            ----b-----
                            ----b-----
                            ----b-----
                            ---xxx----
                            bbbxbxbbb-
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
        , describe "Buffer trail capture and revert"
            [ test "Case 1 (continuation): reverts side strips of the 3 most-recent straight cells"
                (\_ ->
                    let
                        -- Build a 5-cell horizontal road (5,5)..(9,5).
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        -- Force-fix side buffer cells to a known Nature tile id, simulating a
                        -- WFC pass that filled the buffer with Nature.
                        sideBufferCells =
                            cartesian [ 6, 7, 8 ] [ 2, 3, 4, 6, 7, 8 ]

                        withFixedNature =
                            List.foldl (forceFixNatureTile defaultTileId) roadOnly sideBufferCells

                        -- Place the 6th adjacent cell. captureTrail records side strips of
                        -- the 3 most-recent straight cells (cols 7, 8, 9) into the dict but
                        -- leaves the cells as Fixed Nature on the live tilemap.
                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedNature

                        bufferRows =
                            [ 2, 3, 4, 6, 7, 8 ]

                        revertedCells =
                            cartesian [ 7, 8 ] bufferRows

                        -- The actual mutation lives behind revertSavedNature, which
                        -- runs against the WFC.Model fork in startWFC.
                        afterApply =
                            revertSavedNature afterPlacement
                    in
                    Expect.all
                        [ \_ ->
                            -- Live tilemap (post-capture) keeps cells as Fixed Nature.
                            revertedCells
                                |> List.map (cellKindAt afterPlacement)
                                |> List.all (Maybe.map isFixedKind >> Maybe.withDefault False)
                                |> Expect.equal True
                        , \_ ->
                            -- Saved dict matches the captured cells.
                            Dict.keys (getSavedNatureTiles afterPlacement)
                                |> List.sort
                                |> Expect.equalLists (List.sort revertedCells)
                        , \_ ->
                            -- After applying the revert (fork view), cells become Buffer.
                            revertedCells
                                |> List.map (cellKindAt afterApply)
                                |> Expect.equalLists
                                    (List.repeat (List.length revertedCells) (Just Buffer))
                        , \_ ->
                            -- Col 6 (older than the 3-cell window) stays Fixed Nature
                            -- in both phases.
                            cartesian [ 6 ] bufferRows
                                |> List.map (cellKindAt afterApply)
                                |> List.all (Maybe.map isFixedKind >> Maybe.withDefault False)
                                |> Expect.equal True
                        ]
                        ()
                )
            , test "Case 2 (join): walks ≤2 cells along each axis from the placed cell"
                (\_ ->
                    let
                        -- Two 3-cell horizontal stubs separated by a 1-cell gap.
                        stubs =
                            placeRoad
                                [ ( 3, 5 ), ( 4, 5 ), ( 5, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        -- Force-fix the side buffers around both stubs (cols 4 and 8).
                        natureCells =
                            cartesian [ 4, 8 ] [ 2, 3, 4, 6, 7, 8 ]

                        withFixedNature =
                            List.foldl (forceFixNatureTile defaultTileId) stubs natureCells

                        -- Place the joining cell.
                        afterJoin =
                            placeRoad [ ( 6, 5 ) ] withFixedNature

                        afterApply =
                            revertSavedNature afterJoin

                        leftBranch =
                            cartesian [ 4 ] [ 2, 3, 4, 6, 7, 8 ]

                        rightBranch =
                            cartesian [ 8 ] [ 2, 3, 4, 6, 7, 8 ]
                    in
                    Expect.all
                        [ \_ ->
                            -- Live tilemap (post-capture) keeps both branches as Fixed Nature.
                            leftBranch
                                ++ rightBranch
                                |> List.map (cellKindAt afterJoin)
                                |> List.all (Maybe.map isFixedKind >> Maybe.withDefault False)
                                |> Expect.equal True
                        , \_ ->
                            -- Dict captures both branches.
                            Dict.keys (getSavedNatureTiles afterJoin)
                                |> List.sort
                                |> Expect.equalLists (List.sort (leftBranch ++ rightBranch))
                        , \_ ->
                            -- After applying the revert, col 4 (left branch) becomes Buffer.
                            leftBranch
                                |> List.map (cellKindAt afterApply)
                                |> Expect.equalLists (List.repeat 6 (Just Buffer))
                        , \_ ->
                            -- And col 8 (right branch) becomes Buffer.
                            rightBranch
                                |> List.map (cellKindAt afterApply)
                                |> Expect.equalLists (List.repeat 6 (Just Buffer))
                        ]
                        ()
                )
            , test "Skips Fixed cells that belong to a large tile (parentTile = Just _)"
                (\_ ->
                    let
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        -- Force-fix the side buffer at col 7 rows 4 and 6 to Nature, but
                        -- mark col 8 row 4 as a large-tile member that must not be reverted.
                        withFixedNature =
                            roadOnly
                                |> forceFixNatureTile defaultTileId ( 7, 4 )
                                |> forceFixNatureTile defaultTileId ( 7, 6 )

                        largeTileMember =
                            Tile.init
                                (Fixed
                                    { id = defaultTileId
                                    , name = "test-large"
                                    , parentTile = Just ( 999, 1 )
                                    , animation = Nothing
                                    }
                                )

                        protectedCell =
                            createCell constraints 8 4

                        withProtected =
                            mapCell protectedCell (\_ -> largeTileMember) withFixedNature

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withProtected

                        afterApply =
                            revertSavedNature afterPlacement
                    in
                    Expect.all
                        [ \_ ->
                            -- Protected cell is still Fixed both before and after applying.
                            cellKindAt afterApply ( 8, 4 )
                                |> Maybe.map isFixedKind
                                |> Expect.equal (Just True)
                        , \_ ->
                            -- Dict never includes the protected cell.
                            getSavedNatureTiles afterPlacement
                                |> Dict.member ( 8, 4 )
                                |> Expect.equal False
                        , \_ ->
                            -- Ordinary Nature cells were captured into the dict.
                            getSavedNatureTiles afterPlacement
                                |> Dict.member ( 7, 4 )
                                |> Expect.equal True
                        , \_ ->
                            -- After applying the revert, the ordinary Nature cells become Buffer.
                            cellKindAt afterApply ( 7, 4 )
                                |> Expect.equal (Just Buffer)
                        ]
                        ()
                )
            , test "Buffer cells in the trail are a no-op (not added to the saved dict)"
                (\_ ->
                    let
                        -- Build 5 cells so the buffer system populates side Buffer cells
                        -- around cols 6-8. No force-fix, so the trail zone holds Buffer
                        -- (not Fixed Nature).
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        preCheck =
                            cellKindAt roadOnly ( 7, 4 ) == Just Buffer

                        -- Extend to (10,5). Trail covers side strips of cols 7-9; those
                        -- cells are already Buffer, so they must stay Buffer and the
                        -- saved dict must stay empty.
                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] roadOnly

                        trailSideCells =
                            cartesian [ 7, 8 ] [ 2, 3, 4, 6, 7, 8 ]
                    in
                    Expect.all
                        [ \_ -> Expect.equal True preCheck
                        , \tm ->
                            trailSideCells
                                |> List.map (cellKindAt tm)
                                |> Expect.equalLists
                                    (List.repeat (List.length trailSideCells) (Just Buffer))
                        , \tm ->
                            getSavedNatureTiles tm
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        afterPlacement
                )
            , test "reconcileSavedNatureTiles: drops lot-member entries and restores Buffer cells"
                (\_ ->
                    let
                        cellA =
                            createCell constraints 7 3

                        cellB =
                            createCell constraints 7 7

                        bufferLikeTilemap =
                            emptyTilemap
                                |> mapCell cellA (\_ -> Tile.init Buffer)
                                |> mapCell cellB (\_ -> Tile.init Buffer)
                                |> insertSavedNatureTile ( 7, 3 ) defaultTileId
                                |> insertSavedNatureTile ( 7, 7 ) defaultTileId

                        -- Simulate a lot landing at cellA after WFC.
                        lotMember =
                            Tile.init
                                (Fixed
                                    { id = defaultTileId
                                    , name = "test-lot"
                                    , parentTile = Just ( 1234, 0 )
                                    , animation = Nothing
                                    }
                                )

                        beforeReconcile =
                            mapCell cellA (\_ -> lotMember) bufferLikeTilemap

                        afterReconcile =
                            reconcileSavedNatureTiles beforeReconcile
                    in
                    Expect.all
                        [ \tm ->
                            -- Lot member preserved.
                            cellKindAt tm ( 7, 3 )
                                |> Maybe.map isFixedKind
                                |> Expect.equal (Just True)
                        , \tm ->
                            -- Buffer cell restored to Fixed.
                            cellKindAt tm ( 7, 7 )
                                |> Maybe.map isFixedKind
                                |> Expect.equal (Just True)
                        , \tm ->
                            -- Restored cell has the saved id.
                            tileByCell tm cellB
                                |> Maybe.andThen Tile.id
                                |> Expect.equal (Just defaultTileId)
                        , \tm ->
                            getSavedNatureTiles tm
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        afterReconcile
                )
            , test "Removing a road clears the saved dict"
                (\_ ->
                    let
                        cell10 =
                            createCell constraints 10 5

                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        withFixedNature =
                            List.foldl
                                (forceFixNatureTile defaultTileId)
                                roadOnly
                                (cartesian [ 6, 7, 8 ] [ 2, 3, 4, 6, 7, 8 ])

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedNature

                        ( wfcAfterRemove, _ ) =
                            Tilemap.DrivenWFC.onRemoveTile
                                (SeedState.fromSeed testSeed)
                                Dict.empty
                                cell10
                                afterPlacement

                        afterRemoval =
                            Tilemap.WFC.toTilemap wfcAfterRemove
                    in
                    Expect.all
                        [ \_ ->
                            -- Sanity: dict was non-empty before removal.
                            getSavedNatureTiles afterPlacement
                                |> Dict.isEmpty
                                |> Expect.equal False
                        , \_ ->
                            getSavedNatureTiles afterRemoval
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        ()
                )
            , test "Non-adjacent placement clears the saved dict (player moved to a different area)"
                (\_ ->
                    let
                        -- Phase 1: build a road and capture trail entries into the dict.
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        withFixedNature =
                            List.foldl
                                (forceFixNatureTile defaultTileId)
                                roadOnly
                                (cartesian [ 6, 7, 8 ] [ 2, 3, 4, 6, 7, 8 ])

                        afterPhase1 =
                            placeRoad [ ( 10, 5 ) ] withFixedNature

                        -- Phase 2: place far away (non-adjacent to (10,5)).
                        afterPhase2 =
                            placeRoad [ ( 2, 2 ) ] afterPhase1

                        priorDict =
                            getSavedNatureTiles afterPhase1
                    in
                    Expect.all
                        [ \_ ->
                            -- Precondition: phase 1 populated the dict.
                            Dict.isEmpty priorDict
                                |> Expect.equal False
                        , \_ ->
                            -- Non-adjacent placement clears the dict.
                            getSavedNatureTiles afterPhase2
                                |> Dict.isEmpty
                                |> Expect.equal True
                        , \_ ->
                            -- Live cells from phase 1's trail are still Fixed Nature
                            -- (capture never mutated them, and the dict reset doesn't either).
                            cartesian [ 7, 8 ] [ 2, 3, 4, 6, 7, 8 ]
                                |> List.map (cellKindAt afterPhase2)
                                |> List.all (Maybe.map isFixedKind >> Maybe.withDefault False)
                                |> Expect.equal True
                        ]
                        ()
                )
            , test "restartWfc applies the saved-trail revert to its fork (failure-restart parity)"
                (\_ ->
                    let
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        withFixedNature =
                            List.foldl
                                (forceFixNatureTile defaultTileId)
                                roadOnly
                                (cartesian [ 6, 7, 8 ] [ 2, 3, 4, 6, 7, 8 ])

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedNature

                        -- The WFCFailed handler restarts from the live tilemap; the fork
                        -- must include the trail revert just like startWFC's fork does.
                        forkTilemap =
                            Tilemap.DrivenWFC.restartWfc
                                (SeedState.fromSeed testSeed)
                                Dict.empty
                                afterPlacement
                                |> Tilemap.WFC.toTilemap

                        capturedCells =
                            Dict.keys (getSavedNatureTiles afterPlacement)
                    in
                    Expect.all
                        [ \_ ->
                            -- Sanity: the trail was captured.
                            List.isEmpty capturedCells
                                |> Expect.equal False
                        , \_ ->
                            -- Captured cells must not remain Fixed Nature in the fork.
                            capturedCells
                                |> List.filter
                                    (\coords ->
                                        cellKindAt forkTilemap coords
                                            |> Maybe.map isFixedKind
                                            |> Maybe.withDefault False
                                    )
                                |> Expect.equalLists []
                        ]
                        ()
                )
            , test "Distant join placement clears stale entries from the previous area"
                (\_ ->
                    let
                        -- Pre-existing stubs with a 1-cell gap, far from the build area.
                        stubs =
                            placeRoad
                                [ ( 3, 8 ), ( 4, 8 ), ( 6, 8 ), ( 7, 8 ) ]
                                emptyTilemap

                        -- Phase 1: build a road elsewhere and capture trail entries.
                        roadOnly =
                            placeRoad
                                [ ( 2, 2 ), ( 3, 2 ), ( 4, 2 ), ( 5, 2 ), ( 6, 2 ) ]
                                stubs

                        withFixedNature =
                            List.foldl
                                (forceFixNatureTile defaultTileId)
                                roadOnly
                                (cartesian [ 4, 5, 6 ] [ 1, 3 ])

                        afterPhase1 =
                            placeRoad [ ( 7, 2 ) ] withFixedNature

                        -- Phase 2: fill the distant gap. It's a join (2 road neighbors)
                        -- but adjacent to nothing in the build history, so the phase 1
                        -- entries are stale and must be dropped.
                        afterJoin =
                            placeRoad [ ( 5, 8 ) ] afterPhase1
                    in
                    Expect.all
                        [ \_ ->
                            -- Precondition: phase 1 populated the dict.
                            getSavedNatureTiles afterPhase1
                                |> Dict.isEmpty
                                |> Expect.equal False
                        , \_ ->
                            getSavedNatureTiles afterJoin
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        ()
                )
            ]
        , describe "Large nature tile footprint trail"
            [ test "Capture + revert: a force-fixed double partially overlapping the trail reverts its whole footprint"
                (\_ ->
                    let
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        -- NatureDouble1 (1x2) top-left at (8,1) covers (8,1) and (8,2).
                        -- Only (8,2) is inside the trail's captured rows (2-4, 6-8)
                        withFixedDouble =
                            forceFixLargeNatureTile natureDouble1Id ( 8, 1 ) roadOnly

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedDouble

                        footprint =
                            [ ( 8, 1 ), ( 8, 2 ) ]

                        afterApply =
                            revertSavedNature afterPlacement
                    in
                    Expect.all
                        [ \_ ->
                            -- Sanity check: the live tilemap (post-capture) keeps the double intact.
                            footprint
                                |> List.map (cellKindAt afterPlacement)
                                |> List.all (Maybe.map isFixedKind >> Maybe.withDefault False)
                                |> Expect.equal True
                        , \_ ->
                            getSavedNatureAnchors afterPlacement
                                |> Dict.toList
                                |> Expect.equalLists [ ( ( 8, 1 ), natureDouble1Id ) ]
                        , \_ ->
                            footprint
                                |> List.map (cellKindAt afterApply)
                                |> Expect.equalLists [ Just Buffer, Just Buffer ]
                        ]
                        ()
                )
            , test "restartWfc removes Nature-biome Large ids from the reclaimed footprint and its margin, but not beyond it"
                (\_ ->
                    let
                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        withFixedDouble =
                            forceFixLargeNatureTile natureDouble1Id ( 8, 1 ) roadOnly

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedDouble

                        forkTilemap =
                            Tilemap.DrivenWFC.restartWfc
                                (SeedState.fromSeed testSeed)
                                Dict.empty
                                afterPlacement
                                |> Tilemap.WFC.toTilemap

                        -- Footprint (8,1)-(8,2) plus its 1-cell orthogonal margin.
                        marginCells =
                            [ ( 7, 1 ), ( 9, 1 ), ( 7, 2 ), ( 9, 2 ), ( 8, 3 ) ]

                        restrictedCandidates =
                            List.filterMap (optionsAtCell forkTilemap) (( 8, 1 ) :: ( 8, 2 ) :: marginCells)

                        -- Well outside the footprint/margin, but still part of the
                        -- trail's side buffer, so it stays open to Nature Large ids.
                        outsideMarginCell =
                            ( 8, 7 )
                    in
                    Expect.all
                        [ \_ ->
                            -- Sanity check: some margin cells actually ended up in Superposition.
                            List.isEmpty restrictedCandidates
                                |> Expect.equal False
                        , \_ ->
                            restrictedCandidates
                                |> List.concat
                                |> List.any isNatureLargeId
                                |> Expect.equal False
                        , \_ ->
                            optionsAtCell forkTilemap outsideMarginCell
                                |> Maybe.map (List.any isNatureLargeId)
                                |> Expect.equal (Just True)
                        ]
                        ()
                )
            , test "reconcileSavedNatureTiles: reinstates the large tile exactly when its area is unclaimed"
                (\_ ->
                    let
                        -- Simulate a WFC pass that filled the tile area with single cell Nature tiles
                        -- instead of the original double.
                        beforeReconcile =
                            emptyTilemap
                                |> forceFixNatureTile defaultTileId ( 7, 3 )
                                |> forceFixNatureTile defaultTileId ( 7, 4 )
                                |> insertSavedNatureAnchor ( 7, 3 ) natureDouble1Id

                        afterReconcile =
                            reconcileSavedNatureTiles beforeReconcile
                    in
                    Expect.all
                        [ \_ ->
                            -- Sanity check: before reconcile, neither cell is a large-tile member.
                            [ ( 7, 3 ), ( 7, 4 ) ]
                                |> List.map (fixedParentAt beforeReconcile)
                                |> Expect.equalLists [ Nothing, Nothing ]
                        , \_ ->
                            fixedParentAt afterReconcile ( 7, 3 )
                                |> Expect.equal (Just ( natureDouble1Id, 0 ))
                        , \_ ->
                            fixedParentAt afterReconcile ( 7, 4 )
                                |> Expect.equal (Just ( natureDouble1Id, 1 ))
                        , \_ ->
                            getSavedNatureAnchors afterReconcile
                                |> Dict.isEmpty
                                |> Expect.equal True
                        , \_ ->
                            getSavedNatureTiles afterReconcile
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        ()
                )
            , test "reconcileSavedNatureTiles: drops the entry when a lot claims a footprint cell"
                (\_ ->
                    let
                        lotMember =
                            Tile.init
                                (Fixed
                                    { id = defaultTileId
                                    , name = "test-lot"
                                    , parentTile = Just ( lotLargeTileId, 0 )
                                    , animation = Nothing
                                    }
                                )

                        beforeReconcile =
                            emptyTilemap
                                |> mapCell (createCell constraints 7 3) (\_ -> lotMember)
                                |> forceFixNatureTile defaultTileId ( 7, 4 )
                                |> insertSavedNatureAnchor ( 7, 3 ) natureDouble1Id

                        afterReconcile =
                            reconcileSavedNatureTiles beforeReconcile
                    in
                    Expect.all
                        [ \_ ->
                            fixedParentAt afterReconcile ( 7, 3 )
                                |> Expect.equal (Just ( lotLargeTileId, 0 ))
                        , \_ ->
                            -- Partial coverage drops the whole entry.
                            tileByCell afterReconcile (createCell constraints 7 4)
                                |> Maybe.andThen Tile.id
                                |> Expect.equal (Just defaultTileId)
                        , \_ ->
                            getSavedNatureAnchors afterReconcile
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        ()
                )
            , test "Removing a road clears the saved nature anchors dict too"
                (\_ ->
                    let
                        cell10 =
                            createCell constraints 10 5

                        roadOnly =
                            placeRoad
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap

                        withFixedDouble =
                            forceFixLargeNatureTile natureDouble1Id ( 8, 1 ) roadOnly

                        afterPlacement =
                            placeRoad [ ( 10, 5 ) ] withFixedDouble

                        ( wfcAfterRemove, _ ) =
                            Tilemap.DrivenWFC.onRemoveTile
                                (SeedState.fromSeed testSeed)
                                Dict.empty
                                cell10
                                afterPlacement

                        afterRemoval =
                            Tilemap.WFC.toTilemap wfcAfterRemove
                    in
                    Expect.all
                        [ \_ ->
                            getSavedNatureAnchors afterPlacement
                                |> Dict.isEmpty
                                |> Expect.equal False
                        , \_ ->
                            getSavedNatureAnchors afterRemoval
                                |> Dict.isEmpty
                                |> Expect.equal True
                        ]
                        ()
                )
            ]
        ]


cartesian : List Int -> List Int -> List ( Int, Int )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


cellKindAt : Tilemap -> ( Int, Int ) -> Maybe TileKind
cellKindAt tilemap coords =
    let
        cell =
            createCell (getTilemapConfig tilemap) (Tuple.first coords) (Tuple.second coords)
    in
    tileByCell tilemap cell |> Maybe.map .kind


isFixedKind : TileKind -> Bool
isFixedKind kind =
    case kind of
        Fixed _ ->
            True

        _ ->
            False


fixedParentAt : Tilemap -> ( Int, Int ) -> Maybe ( TileConfig.TileId, Int )
fixedParentAt tilemap coords =
    case cellKindAt tilemap coords of
        Just (Fixed props) ->
            props.parentTile

        _ ->
            Nothing


optionsAtCell : Tilemap -> ( Int, Int ) -> Maybe (List TileConfig.TileId)
optionsAtCell tilemap coords =
    case cellKindAt tilemap coords of
        Just (Superposition options) ->
            Just options

        _ ->
            Nothing


isNatureLargeId : TileConfig.TileId -> Bool
isNatureLargeId tileId =
    case tileById tileId of
        TileConfig.Large largeTile ->
            largeTile.biome == TileConfig.Nature

        TileConfig.Single _ ->
            False
