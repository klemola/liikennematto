module DrivenWFCTests exposing (suite)

import Data.TileSet exposing (tileById)
import Data.Utility
    exposing
        ( createCell
        , placeRoad
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        , tilemapFromCoordinates
        , worldFromSavegame
        )
import Data.Worlds exposing (worldWithSchool)
import Dict
import Expect
import Lib.FSM as FSM
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Lib.SeedState as SeedState
import Maybe.Extra as Maybe
import Random
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , createTilemap
        , fixedTileByCell
        , foldTiles
        , getSavedNatureTiles
        , insertSavedNatureTile
        , setSuperpositionOptions
        , tileByCell
        , tileNeighborIn
        )
import Tilemap.DrivenWFC
    exposing
        ( addTileById
        , bufferToSuperposition
        , onRemoveTile
        , pruneUnfittableLargeTiles
        , restartWfc
        )
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig as TileConfig exposing (TileId)
import Tilemap.WFC as WFC


constraints : Cell.Constraints {}
constraints =
    tenByTenTilemap


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap constraints (\_ -> Tile.init Tile.Unintialized)


tileSuperposition : Tile -> List TileId
tileSuperposition tile =
    case tile.kind of
        Tile.Superposition opts ->
            opts

        _ ->
            []


cellHasTile : Cell -> TileId -> Tilemap -> Expect.Expectation
cellHasTile cell expectedTileId tilemap =
    case fixedTileByCell tilemap cell |> Maybe.andThen Tile.id of
        Just actualTileId ->
            Expect.equal actualTileId expectedTileId

        Nothing ->
            Expect.fail ("Tile not found at " ++ Cell.toString cell)


cellTileIsBeingRemoved : Cell -> Tilemap -> Expect.Expectation
cellTileIsBeingRemoved cell tilemap =
    case tileByCell tilemap cell of
        Just tile ->
            Expect.equal (FSM.toCurrentState tile.fsm) Tile.Removing

        Nothing ->
            Expect.fail ("Tile not found at " ++ Cell.toString cell)


neighborCellHasTile : Cell -> OrthogonalDirection -> Tile.TileKind -> Tilemap -> Expect.Expectation
neighborCellHasTile origin dir expectedTileKind tilemap =
    case tileNeighborIn dir origin tileByCell tilemap of
        Just ( _, neighborTile ) ->
            Expect.equal neighborTile.kind expectedTileKind

        Nothing ->
            Expect.fail
                (String.join " "
                    [ "Neighbor not found in dir"
                    , OrthogonalDirection.toString dir
                    , "from"
                    , Cell.toString origin
                    ]
                )


fixedTileProps id name =
    { id = id
    , name = name
    , parentTile = Nothing
    , animation = Nothing
    }


suite : Test
suite =
    describe "Tilemap.DrivenWFC"
        [ describe ".addTileById"
            [ test "Should add a tile by bitmask - one neighbor changed"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ) ]
                                emptyTilemap

                        newCell =
                            createCell constraints 9 5

                        deadendRightId =
                            5

                        ( tilemapWithTile, _ ) =
                            addTileById (SeedState.fromSeed testSeed) Dict.empty newCell deadendRightId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendRightId
                        , neighborCellHasTile newCell OrthogonalDirection.Up Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Right Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Down Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 2 "RoadHorizontal"))
                        ]
                        tilemapWithTile
                )
            , test "Should add a tile by bitmask - all neighbors changed"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                -- This is a nearly-finished intersection that is missing the center tile
                                [ ( 5, 5 ), ( 6, 5 ), ( 8, 5 ), ( 9, 5 ), ( 7, 4 ), ( 7, 6 ) ]
                                emptyTilemap

                        newCell =
                            createCell constraints 7 5

                        intersectionCrossId =
                            16

                        ( tilemapWithTile, _ ) =
                            addTileById (SeedState.fromSeed testSeed) Dict.empty newCell intersectionCrossId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell intersectionCrossId
                        , neighborCellHasTile newCell OrthogonalDirection.Up (Tile.Fixed (fixedTileProps 4 "RoadDeadendUp"))
                        , neighborCellHasTile newCell OrthogonalDirection.Right (Tile.Fixed (fixedTileProps 2 "RoadHorizontal"))
                        , neighborCellHasTile newCell OrthogonalDirection.Down (Tile.Fixed (fixedTileProps 6 "RoadDeadendDown"))
                        , neighborCellHasTile newCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 2 "RoadHorizontal"))
                        ]
                        tilemapWithTile
                )
            , test "Should add a tile by bitmask - neighbor changes to accomodate a lot"
                (\_ ->
                    let
                        tilemap =
                            worldWithSchool.tilemap

                        newCell =
                            createCell constraints 4 4

                        deadendLeftId =
                            7

                        ( tilemapWithTile, _ ) =
                            addTileById (SeedState.fromSeed testSeed) Dict.empty newCell deadendLeftId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendLeftId
                        , neighborCellHasTile newCell
                            OrthogonalDirection.Right
                            (Tile.Fixed (fixedTileProps 43 "RoadIntersectionTLeftLotEntryRight"))
                        ]
                        tilemapWithTile
                )
            , test "Should add a tile by bitmask - ignore lot neighbor if driveway is facing the other way"
                (\_ ->
                    let
                        tilemap =
                            worldWithSchool.tilemap

                        newCell =
                            createCell constraints 6 6

                        deadendDownId =
                            6

                        ( tilemapWithTile, _ ) =
                            addTileById (SeedState.fromSeed testSeed) Dict.empty newCell deadendDownId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendDownId
                        , neighborCellHasTile newCell
                            OrthogonalDirection.Up
                            (Tile.Fixed (fixedTileProps 13 "RoadIntersectionTDown"))
                        ]
                        tilemapWithTile
                )
            ]
        , describe ".onRemoveTile"
            [ test "Should remove a tile - one neighbor changed"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ) ]
                                emptyTilemap

                        removedCell =
                            createCell constraints 8 5

                        ( wfcModel, _ ) =
                            onRemoveTile (SeedState.fromSeed testSeed) Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell OrthogonalDirection.Up Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Right Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Down Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 5 "RoadDeadendRight"))
                        ]
                        tilemapWithoutTile
                )
            , test "Should remove a tile - all neighbors changed"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ), ( 7, 4 ), ( 7, 3 ), ( 7, 6 ), ( 7, 7 ) ]
                                emptyTilemap

                        removedCell =
                            createCell constraints 7 5

                        ( wfcModel, _ ) =
                            onRemoveTile (SeedState.fromSeed testSeed) Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell OrthogonalDirection.Up (Tile.Fixed (fixedTileProps 6 "RoadDeadendDown"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Right (Tile.Fixed (fixedTileProps 7 "RoadDeadendLeft"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Down (Tile.Fixed (fixedTileProps 4 "RoadDeadendUp"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 5 "RoadDeadendRight"))
                        ]
                        tilemapWithoutTile
                )
            , test "Should remove a tile - neighbor changes to accomodate a lot"
                (\_ ->
                    let
                        tilemap =
                            worldWithSchool.tilemap

                        removedCell =
                            createCell constraints 5 5

                        ( wfcModel, _ ) =
                            -- The lot entry becames a deadend "cul-de-sac" to keep the lot entry
                            onRemoveTile (SeedState.fromSeed testSeed) Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell
                            OrthogonalDirection.Up
                            (Tile.Fixed (fixedTileProps 28 "RoadDeadendDownLotEntryRight"))
                        ]
                        tilemapWithoutTile
                )
            ]
        , describe ".restartWfc"
            [ test "Should set fixed roads to superposition - horizontal roads"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ), ( 10, 5 ) ]
                                emptyTilemap
                                |> restartWfc (SeedState.fromSeed testSeed) Dict.empty
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 6 5)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        secondHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 7 5)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        lastHorizontalCellOptions =
                            tileByCell tilemap (createCell constraints 9 5)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        horizontalRoadId =
                            2

                        horizontalRoadLotEntryUpId =
                            17
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions [ horizontalRoadId, horizontalRoadLotEntryUpId ]
                        , \_ -> Expect.equal secondHorizontalRoadCellOptions [ horizontalRoadId, horizontalRoadLotEntryUpId ]
                        , \_ -> Expect.equal lastHorizontalCellOptions []
                        ]
                        ()
                )
            , test "Should set fixed roads to superposition - vertical roads"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 5, 6 ), ( 5, 7 ), ( 5, 8 ), ( 5, 9 ), ( 5, 10 ) ]
                                emptyTilemap
                                |> restartWfc (SeedState.fromSeed testSeed) Dict.empty
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 6)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        lastHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 9)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        verticalRoadId =
                            3

                        verticalRoadLotEntryLeftId =
                            20

                        verticalRoadLotEntryRightId =
                            19
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions []
                        , \_ ->
                            Expect.equal lastHorizontalRoadCellOptions
                                [ verticalRoadId
                                , verticalRoadLotEntryLeftId
                                , verticalRoadLotEntryRightId
                                ]
                        ]
                        ()
                )
            , test "Should set fixed roads to superposition - vertical roads, one side only"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 2, 5 ), ( 2, 6 ), ( 2, 7 ), ( 2, 8 ), ( 2, 9 ), ( 2, 10 ) ]
                                emptyTilemap
                                |> restartWfc (SeedState.fromSeed testSeed) Dict.empty
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 2 6)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        lastHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 2 9)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        verticalRoadId =
                            3

                        verticalRoadLotEntryRightId =
                            19
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions []
                        , \_ -> Expect.equal lastHorizontalRoadCellOptions [ verticalRoadId, verticalRoadLotEntryRightId ]
                        ]
                        ()
                )
            , test "Should not reopen the road if there are no potential driveway neighbors"
                (\_ ->
                    let
                        tilemap =
                            tilemapFromCoordinates
                                constraints
                                [ ( 5, 5 ), ( 5, 6 ), ( 5, 7 ), ( 5, 8 ) ]
                                |> restartWfc (SeedState.fromSeed testSeed) Dict.empty
                                |> WFC.toTilemap
                    in
                    Expect.all
                        [ \_ ->
                            tileByCell tilemap (createCell constraints 5 6)
                                |> Maybe.unwrap False Tile.isFixed
                                |> Expect.equal True
                                |> Expect.onFail "tile remains fixed"
                        ]
                        ()
                )
            ]
        , describe "road tile stability"
            [ test "Road neighbor should remain Fixed even when an adjacent Superposition tile has incompatible options"
                (\_ ->
                    let
                        tilemapWithRoads =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ) ]
                                emptyTilemap

                        -- Surgically replace the Buffer at (5,6) with Superposition [verticalRoad].
                        -- WFC propagation from (5,5) DOWN to (5,6) will find no valid options (incompatible sockets)
                        -- triggering the backtrack cascade.
                        verticalRoadId =
                            3

                        tilemapWithIncompatibleNeighbor =
                            setSuperpositionOptions
                                (createCell constraints 5 6)
                                [ verticalRoadId ]
                                tilemapWithRoads

                        -- Place a new road at (5,4), directly above (5,5).
                        -- processTileNeighbor recollpases (5,5), then propagates DOWN to (5,6).
                        -- (5,5) ends up as Superposition [] instead of Fixed
                        deadendUpId =
                            4

                        ( resultTilemap, _ ) =
                            addTileById
                                (SeedState.fromSeed testSeed)
                                Dict.empty
                                (createCell constraints 5 4)
                                deadendUpId
                                tilemapWithIncompatibleNeighbor

                        neighborCell =
                            createCell constraints 5 5
                    in
                    tileByCell resultTilemap neighborCell
                        |> Maybe.unwrap False Tile.isFixed
                        |> Expect.equal True
                        |> Expect.onFail
                            ("Road at (5,5) should remain Fixed after addTileById, "
                                ++ "but the WFC cascade backtrack left it as Superposition"
                            )
                )
            ]
        , describe "stale saved-nature capture (Bug 5)"
            [ test "Roads built over previously captured nature survive a full WFC cycle"
                (\_ ->
                    case worldFromSavegame incidentBaseSavegame of
                        Err decodeError ->
                            Expect.fail ("Savegame fixture failed to decode: " ++ decodeError)

                        Ok world ->
                            let
                                -- Drag a leg down from the road; the trail side strips of the
                                -- leg cells (rows 4 and 5, x 2..8) capture the nature at
                                -- (8,4) and (8,5)
                                afterLeg =
                                    placeRoad [ ( 5, 4 ), ( 5, 5 ), ( 5, 6 ) ] world.tilemap

                                -- Continue the same build phase along row 6, then up over the
                                -- captured nature cells, closing a ring back to (8,3)
                                afterOverbuild =
                                    placeRoad
                                        [ ( 6, 6 ), ( 7, 6 ), ( 8, 6 ), ( 8, 5 ), ( 8, 4 ) ]
                                        afterLeg

                                overbuiltCells =
                                    [ ( 8, 4 ), ( 8, 5 ) ]

                                capturedEntries =
                                    getSavedNatureTiles afterLeg
                                        |> Dict.keys
                                        |> List.filter (\coords -> List.member coords overbuiltCells)

                                -- Building over the captured cells drops their entries;
                                -- re-add them to prove the revert and reconcile guards
                                -- hold even if staleness slips in through some other path
                                withStaleEntries =
                                    afterOverbuild
                                        |> insertSavedNatureTile ( 8, 4 ) natureSingle1Id
                                        |> insertSavedNatureTile ( 8, 5 ) natureSingle1Id

                                -- Failed runs restart with the next seed from the same
                                -- tilemap, like the WFCFailed handler does. Failures here
                                -- are unrelated churn (large tiles vs. tight buffers on a
                                -- small map); the bug makes every attempt fail
                                solvedModel =
                                    solveWithRetries 5 (SeedState.fromSeed testSeed) withStaleEntries

                                solvedTilemap =
                                    WFC.toTilemap solvedModel

                                biomeAt tilemap ( x, y ) =
                                    tileByCell tilemap (createCell constraints x y)
                                        |> Maybe.andThen Tile.id
                                        |> Maybe.map (tileById >> TileConfig.biome)

                                emptySuperpositionCells tilemap =
                                    foldTiles
                                        (\cell tile acc ->
                                            case tile.kind of
                                                Tile.Superposition [] ->
                                                    Cell.coordinates cell :: acc

                                                _ ->
                                                    acc
                                        )
                                        []
                                        tilemap
                            in
                            Expect.all
                                [ \_ ->
                                    case WFC.currentState solvedModel of
                                        WFC.Failed _ ->
                                            Expect.fail
                                                ("WFC still failing after retries. Log tail: "
                                                    ++ Debug.toString (List.take 8 (WFC.log solvedModel))
                                                )

                                        _ ->
                                            Expect.pass
                                , \_ ->
                                    List.sort capturedEntries
                                        |> Expect.equalLists overbuiltCells
                                        |> Expect.onFail
                                            "Precondition: the trail should have captured (8,4) and (8,5) while they held nature"
                                , \_ ->
                                    getSavedNatureTiles afterOverbuild
                                        |> Dict.keys
                                        |> List.filter (\coords -> List.member coords overbuiltCells)
                                        |> Expect.equalLists []
                                        |> Expect.onFail
                                            "Building over the captured cells should drop their saved entries"
                                , \_ ->
                                    overbuiltCells
                                        |> List.map (biomeAt afterOverbuild)
                                        |> Expect.equalLists [ Just TileConfig.Road, Just TileConfig.Road ]
                                        |> Expect.onFail
                                            "Precondition: (8,4) and (8,5) should hold Fixed roads before the WFC run"
                                , \_ ->
                                    overbuiltCells
                                        |> List.map (biomeAt solvedTilemap)
                                        |> Expect.equalLists [ Just TileConfig.Road, Just TileConfig.Road ]
                                        |> Expect.onFail
                                            "Player-built roads must survive the WFC run despite the stale saved-nature entries"
                                , \_ ->
                                    emptySuperpositionCells solvedTilemap
                                        |> Expect.equalLists []
                                        |> Expect.onFail
                                            "The solved tilemap must not contain empty superpositions"
                                ]
                                ()
                )
            ]
        , describe "seed propagation"
            [ test "WFC internal seed changes during solving"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ), ( 10, 5 ) ]
                                emptyTilemap

                        ( wfcModel, _ ) =
                            restartWfc (SeedState.fromSeed testSeed) Dict.empty tilemap
                                |> WFC.solve
                                |> WFC.flushPendingActions

                        initialSeed =
                            testSeed

                        finalSeedState =
                            WFC.currentSeed wfcModel

                        ( val1, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) initialSeed

                        ( val2, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) finalSeedState.currentSeed
                    in
                    Expect.notEqual val1 val2
                )
            , test "runWfc returns updated seed in WFCSolved"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ), ( 10, 5 ) ]
                                emptyTilemap

                        initialWfc =
                            restartWfc (SeedState.fromSeed testSeed) Dict.empty tilemap

                        ( _, drivenWfcResult, _ ) =
                            Tilemap.DrivenWFC.runWfc 0 tilemap (WFC.solve initialWfc)

                        currentSeed =
                            case drivenWfcResult of
                                Tilemap.DrivenWFC.WFCSolved _ _ _ seedState ->
                                    seedState.currentSeed

                                _ ->
                                    Random.initialSeed 0

                        ( val1, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) testSeed

                        ( val2, _ ) =
                            Random.step (Random.int Random.minInt Random.maxInt) currentSeed
                    in
                    Expect.notEqual val1 val2
                        |> Expect.onFail "runWfc should return updated seed in WFCSolved"
                )
            ]
        , describe ".pruneUnfittableLargeTiles"
            [ test "Prunes Nature 2x2 quads from 1-wide buffer strips"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ) ]
                                emptyTilemap
                                |> bufferToSuperposition
                                |> pruneUnfittableLargeTiles

                        bufferCellOptions y =
                            tileByCell tilemap (createCell constraints 6 y)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        hasAny tileIds options =
                            List.any (\id -> List.member id tileIds) options

                        quadIds =
                            [ natureQuad1Id, natureQuad2Id ]
                    in
                    Expect.all
                        [ \_ ->
                            bufferCellOptions 2
                                |> hasAny quadIds
                                |> Expect.equal False
                                |> Expect.onFail "NatureQuads should be pruned at (6,2)"
                        , \_ ->
                            bufferCellOptions 3
                                |> hasAny quadIds
                                |> Expect.equal False
                                |> Expect.onFail "NatureQuads should be pruned at (6,3)"
                        , \_ ->
                            bufferCellOptions 7
                                |> hasAny quadIds
                                |> Expect.equal False
                                |> Expect.onFail "NatureQuads should be pruned at (6,7)"
                        , \_ ->
                            bufferCellOptions 3
                                |> List.member natureSingle1Id
                                |> Expect.equal True
                                |> Expect.onFail "Nature single tiles should be retained"
                        ]
                        ()
                )
            , test "Retains Nature 2x2 quads at anchors where they fit"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ) ]
                                emptyTilemap
                                |> bufferToSuperposition
                                |> pruneUnfittableLargeTiles

                        -- Anchor at (6, 2) covers cells (6,2)(7,2)(6,3)(7,3), all buffer
                        fitAnchorOptions =
                            tileByCell tilemap (createCell constraints 6 2)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        -- Anchor at (8, 2) covers (8,2)(9,2)(8,3)(9,3). X=9 is not buffer,
                        noFitAnchorOptions =
                            tileByCell tilemap (createCell constraints 8 2)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []
                    in
                    Expect.all
                        [ \_ ->
                            List.member natureQuad1Id fitAnchorOptions
                                |> Expect.equal True
                                |> Expect.onFail "NatureQuad1 should be kept at (6,2) where it fits"
                        , \_ ->
                            List.member natureQuad2Id fitAnchorOptions
                                |> Expect.equal True
                                |> Expect.onFail "NatureQuad2 should be kept at (6,2) where it fits"
                        , \_ ->
                            List.member natureQuad1Id noFitAnchorOptions
                                |> Expect.equal False
                                |> Expect.onFail "NatureQuad1 should be pruned at (8,2) where it doesn't fit"
                        , \_ ->
                            List.member natureQuad2Id noFitAnchorOptions
                                |> Expect.equal False
                                |> Expect.onFail "NatureQuad2 should be pruned at (8,2) where it doesn't fit"
                        ]
                        ()
                )
            ]
        ]


natureSingle1Id : TileId
natureSingle1Id =
    211


solveWithRetries : Int -> SeedState.SeedState -> Tilemap -> WFC.Model
solveWithRetries attempts seedState tilemap =
    let
        solved =
            restartWfc seedState Dict.empty tilemap
                |> WFC.solve
    in
    if attempts <= 1 then
        solved

    else
        case WFC.currentState solved of
            WFC.Failed _ ->
                solveWithRetries (attempts - 1) (WFC.currentSeed solved) tilemap

            _ ->
                solved


{-| 10x10 base world: a horizontal road on row 3 (x 4..8) and WFC-collapsed
nature singles below its right end at (8,4) and (8,5). Mirrors the state that
preceded the wfc-fail-blowup incident, before the player built a road leg over
the nature tiles.
-}
incidentBaseSavegame : String
incidentBaseSavegame =
    """
    { "v": 1
    , "seed": [42, 0]
    , "tmd": [10, 10]
    , "tilemap": [ 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,7,2,2,2,5,0,0
                 , 0,0,0,0,0,0,0,211,0,0
                 , 0,0,0,0,0,0,0,212,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 , 0,0,0,0,0,0,0,0,0,0
                 ]
    , "lots": []
    , "nature": []
    }
    """


natureQuad1Id : TileId
natureQuad1Id =
    217


natureQuad2Id : TileId
natureQuad2Id =
    218
