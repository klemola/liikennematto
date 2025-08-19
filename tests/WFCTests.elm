module WFCTests exposing (suite)

import Data.TileSet
    exposing
        ( allTiles
        , defaultTiles
        , threeByThreeLotLeftLargeTile
        , threeByTwoLotUpLargeTile
        , twoByTwoLotRightLargeTile
        )
import Data.Utility
    exposing
        ( createCell
        , initTileWithSuperposition
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        )
import Dict
import Expect
import Random
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell
import Tilemap.Core
    exposing
        ( Tilemap
        , createTilemap
        , foldTiles
        , setSuperpositionOptions
        , tileByCell
        )
import Tilemap.DrivenWFC as DrivenWFC
import Tilemap.Tile as Tile exposing (TileKind(..))
import Tilemap.TileConfig as TileConfig
import Tilemap.TileInventory exposing (TileInventory)
import Tilemap.WFC as WFC


constraints : Cell.Constraints {}
constraints =
    tenByTenTilemap


emptyTilemap : Tilemap
emptyTilemap =
    createTilemap constraints (\_ -> Tile.init Tile.Unintialized)


emptyTilemapWithInitializedCells : Tilemap
emptyTilemapWithInitializedCells =
    createTilemap constraints (initTileWithSuperposition constraints defaultTiles)


testTileInventory : TileInventory Int
testTileInventory =
    allTiles
        |> List.filterMap
            (\tileConfig ->
                case tileConfig of
                    TileConfig.Large largeTile ->
                        if largeTile.biome == TileConfig.Lot then
                            Just largeTile.id

                        else
                            Nothing

                    TileConfig.Single _ ->
                        Nothing
            )
        |> List.map (\tileId -> ( tileId, 2 ))
        |> Dict.fromList


suite : Test
suite =
    describe "Tilemap.WFC"
        [ test "fromTilemap creates a valid initial model"
            (\_ ->
                let
                    tilemap =
                        emptyTilemap

                    model =
                        WFC.fromTilemap tilemap testSeed
                in
                Expect.equal (WFC.currentState model) WFC.Solving
            )
        , test "solve completes successfully for a small tilemap"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    model =
                        WFC.fromTilemap tilemap testSeed
                            |> WFC.withTileInventory testTileInventory
                            |> WFC.solve
                in
                Expect.equal (WFC.currentState model) WFC.Done
            )
        , test "collapse reduces superposition options"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    initialModel =
                        WFC.fromTilemap tilemap testSeed
                            |> WFC.withTileInventory testTileInventory

                    cell =
                        Data.Utility.createCell Data.Utility.tenByTenTilemap 1 1

                    ( collapsedModel, _ ) =
                        WFC.collapse cell initialModel

                    initialOptions =
                        tileByCell (WFC.toTilemap initialModel) cell
                            |> Maybe.andThen
                                (\tile ->
                                    case tile.kind of
                                        Superposition opts ->
                                            Just (List.length opts)

                                        _ ->
                                            Nothing
                                )

                    collapsedOptions =
                        tileByCell (WFC.toTilemap collapsedModel) cell
                            |> Maybe.andThen
                                (\tile ->
                                    case tile.kind of
                                        Superposition opts ->
                                            Just (List.length opts)

                                        Fixed _ ->
                                            Just 1

                                        _ ->
                                            Nothing
                                )
                in
                Maybe.map2 Expect.lessThan initialOptions collapsedOptions
                    |> Maybe.withDefault (Expect.fail "Invalid initial options or collapsed options")
            )
        , test "propagateConstraints updates neighboring cells"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    initialModel =
                        WFC.fromTilemap tilemap testSeed
                            |> WFC.withTileInventory testTileInventory

                    cell =
                        Data.Utility.createCell Data.Utility.tenByTenTilemap 1 1

                    ( collapsedModel, collapsedTileConfig ) =
                        WFC.collapse cell initialModel

                    propagatedModel =
                        WFC.propagateConstraints cell collapsedModel

                    initialNeighborOpts =
                        tileByCell (WFC.toTilemap initialModel) (Data.Utility.createCell Data.Utility.tenByTenTilemap 2 1)
                            |> Maybe.andThen
                                (\initial ->
                                    case initial.kind of
                                        Superposition opts ->
                                            Just opts

                                        _ ->
                                            Nothing
                                )

                    updatedNeighborOpts =
                        tileByCell (WFC.toTilemap propagatedModel) (Data.Utility.createCell Data.Utility.tenByTenTilemap 2 1)
                            |> Maybe.andThen
                                (\initial ->
                                    case initial.kind of
                                        Superposition opts ->
                                            Just opts

                                        _ ->
                                            Nothing
                                )
                in
                Expect.all
                    [ \_ -> Expect.notEqual Nothing collapsedTileConfig
                    , \_ ->
                        Maybe.map2 Expect.greaterThan
                            (Maybe.map List.length updatedNeighborOpts)
                            (Maybe.map List.length initialNeighborOpts)
                            |> Maybe.withDefault (Expect.fail "Invalid initial neighbor opts or updated neighbor opts")
                    ]
                    ()
            )
        , test "step progresses the solving process"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    initialModel =
                        WFC.fromTilemap tilemap testSeed
                            |> WFC.withTileInventory testTileInventory

                    steppedModel =
                        WFC.stepN WFC.StopAtSolved 5 initialModel

                    initialSuperpositions =
                        countSuperpositions (WFC.toTilemap initialModel)

                    steppedSuperpositions =
                        countSuperpositions (WFC.toTilemap steppedModel)
                in
                Expect.lessThan initialSuperpositions steppedSuperpositions
            )
        , test "solve handles backtracking (bad tile inventory)"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    reducedTileInventory =
                        Dict.map (\_ _ -> 0) testTileInventory

                    seed =
                        -- This is a special seed that (currently) guarantees at least one lot generated
                        Random.initialSeed 60

                    model =
                        WFC.fromTilemap tilemap seed
                            |> WFC.withTileInventory reducedTileInventory
                            |> WFC.solve

                    wfcContext =
                        WFC.contextDebug model
                in
                Expect.all
                    [ \_ -> Expect.equal (WFC.currentState model) WFC.Done
                    , \_ -> Expect.atLeast 1 wfcContext.backtrackCount
                    ]
                    ()
            )
        , test "solve handles backtracking (bad tile pick)"
            (\_ ->
                let
                    tilemap =
                        emptyTilemapWithInitializedCells

                    cell =
                        createCell constraints 2 2

                    threeByThreeLotLeftId =
                        114

                    model =
                        WFC.fromTilemap tilemap testSeed
                            |> WFC.withTileInventory testTileInventory
                            |> WFC.debug_collapseWithId cell threeByThreeLotLeftId
                            |> WFC.step WFC.StopAtEmptySteps

                    wfcContext =
                        WFC.contextDebug model
                in
                Expect.atLeast 1 wfcContext.backtrackCount
            )
        , describe ".checkLargeTileFit"
            [ test "Should find fitting tiles (vertical road)"
                (\_ ->
                    let
                        twoByTwoLotDriveway =
                            createCell constraints 2 4

                        twoByTwoLotEntry =
                            createCell constraints 3 4

                        threeByThreeLotDriveway =
                            createCell constraints 4 5

                        threeByThreeLotEntry =
                            createCell constraints 3 5

                        verticalRoadId =
                            2

                        verticalRoadLotEntryLeftId =
                            18

                        verticalRoadLotEntryRightId =
                            17

                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 3, 1 ), ( 3, 2 ), ( 3, 3 ), ( 3, 4 ), ( 3, 5 ), ( 3, 6 ) ]
                                emptyTilemap
                                |> setSuperpositionOptions twoByTwoLotEntry [ verticalRoadLotEntryLeftId, verticalRoadId ]
                                |> setSuperpositionOptions threeByThreeLotEntry [ verticalRoadLotEntryRightId, verticalRoadId ]
                                |> DrivenWFC.bufferToSuperposition
                    in
                    Expect.all
                        [ \_ ->
                            Expect.equal
                                (WFC.checkLargeTileFit tilemap twoByTwoLotDriveway twoByTwoLotRightLargeTile)
                                (Just twoByTwoLotRightLargeTile)
                        , \_ ->
                            Expect.equal
                                (WFC.checkLargeTileFit tilemap threeByThreeLotDriveway threeByThreeLotLeftLargeTile)
                                (Just threeByThreeLotLeftLargeTile)
                        ]
                        ()
                )
            , test "Should find fitting tiles (horizontal road)"
                (\_ ->
                    let
                        threeByTwoLotDriveway =
                            createCell constraints 2 3

                        threeByTwoLotEntry =
                            createCell constraints 2 4

                        horizontalRoadId =
                            1

                        horizontalRoadLotEntryUpId =
                            16

                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 1, 4 ), ( 2, 4 ), ( 3, 4 ), ( 4, 4 ), ( 5, 4 ) ]
                                emptyTilemap
                                |> setSuperpositionOptions threeByTwoLotEntry [ horizontalRoadLotEntryUpId, horizontalRoadId ]
                                |> DrivenWFC.bufferToSuperposition
                    in
                    Expect.equal
                        (WFC.checkLargeTileFit tilemap threeByTwoLotDriveway threeByTwoLotUpLargeTile)
                        (Just threeByTwoLotUpLargeTile)
                )
            , test "Should report bad tile fit (fixed tile blocks lot entry)"
                (\_ ->
                    let
                        threeByTwoLotDriveway =
                            createCell constraints 2 3

                        threeByTwoLotEntry =
                            createCell constraints 2 4

                        horizontalRoadId =
                            1

                        horizontalRoadLotEntryUpId =
                            16

                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 1, 4 ), ( 2, 4 ), ( 3, 4 ), ( 4, 4 ), ( 5, 4 ), ( 2, 2 ) ]
                                emptyTilemap
                                |> setSuperpositionOptions threeByTwoLotEntry [ horizontalRoadLotEntryUpId, horizontalRoadId ]
                    in
                    Expect.equal
                        (WFC.checkLargeTileFit tilemap threeByTwoLotDriveway threeByTwoLotUpLargeTile)
                        Nothing
                )
            ]
        ]


countSuperpositions : Tilemap -> Int
countSuperpositions tilemap =
    foldTiles
        (\_ tile count ->
            case tile.kind of
                Superposition _ ->
                    count + 1

                _ ->
                    count
        )
        0
        tilemap
