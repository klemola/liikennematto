module SavegameTests exposing (suite)

import Data.Lots as Lots exposing (NewLot)
import Data.TileSet
import Data.Worlds as Worlds
import Dict exposing (Dict)
import Expect
import Json.Encode as JE
import Lib.Collection as Collection
import Lib.FSM as FSM
import Model.World as World exposing (World)
import Random
import Savegame
import Simulation.RoadNetwork as RoadNetwork
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell exposing (CellCoordinates)
import Tilemap.Core as Tilemap
import Tilemap.Tile as Tile
import Tilemap.TileConfig as TileConfig exposing (LargeTile, TileId)


suite : Test
suite =
    describe "Savegame"
        [ encodeTests
        , decodeTests
        , roundTripTests
        , invalidInputTests
        , parentTileComputationTests
        , seedDeterminismTests
        ]


encodeTests : Test
encodeTests =
    describe "Encoding"
        [ test "encodes simple world to JSON with all required fields"
            (\_ ->
                let
                    world =
                        Worlds.simpleWorld

                    json =
                        Savegame.encode world

                    jsonString =
                        JE.encode 0 json
                in
                Expect.all
                    [ \_ -> jsonString |> String.contains "\"v\"" |> Expect.equal True
                    , \_ -> jsonString |> String.contains "\"seed\"" |> Expect.equal True
                    , \_ -> jsonString |> String.contains "\"tmd\"" |> Expect.equal True
                    , \_ -> jsonString |> String.contains "\"tilemap\"" |> Expect.equal True
                    , \_ -> jsonString |> String.contains "\"lots\"" |> Expect.equal True
                    ]
                    ()
            )
        , test "encodes tilemap dimensions correctly"
            (\_ ->
                let
                    world =
                        Worlds.defaultWorld

                    json =
                        Savegame.encode world

                    jsonString =
                        JE.encode 0 json
                in
                -- tmd is now an array [width, height]
                Expect.all
                    [ \_ -> jsonString |> String.contains "\"tmd\":[10,10]" |> Expect.equal True
                    ]
                    ()
            )
        , test "encodes world with school lot"
            (\_ ->
                let
                    world =
                        Worlds.worldWithSchool

                    json =
                        Savegame.encode world

                    jsonString =
                        JE.encode 0 json
                in
                -- lots are now arrays [lotId, x, y] where LotSchool has ID 2
                Expect.all
                    [ \_ -> jsonString |> String.contains "\"lots\"" |> Expect.equal True
                    , \_ -> jsonString |> String.contains "[2," |> Expect.equal True
                    ]
                    ()
            )
        ]


decodeTests : Test
decodeTests =
    describe "Decoding"
        [ test "decodes valid simple world JSON"
            (\_ ->
                let
                    originalWorld =
                        Worlds.simpleWorld

                    json =
                        Savegame.encode originalWorld

                    result =
                        Savegame.decode json
                in
                case result of
                    Ok decodedWorld ->
                        let
                            decodedConfig =
                                Tilemap.getTilemapConfig decodedWorld.tilemap

                            originalConfig =
                                Tilemap.getTilemapConfig originalWorld.tilemap
                        in
                        Expect.all
                            [ \_ ->
                                decodedConfig.horizontalCellsAmount
                                    |> Expect.equal originalConfig.horizontalCellsAmount
                            , \_ ->
                                decodedConfig.verticalCellsAmount
                                    |> Expect.equal originalConfig.verticalCellsAmount
                            ]
                            ()

                    Err error ->
                        Expect.fail ("Decoding failed: " ++ error)
            )
        , test "decodes world with school lot correctly"
            (\_ ->
                let
                    originalWorld =
                        Worlds.worldWithSchool

                    json =
                        Savegame.encode originalWorld

                    result =
                        Savegame.decode json
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalLotsCount =
                                Collection.size originalWorld.lots

                            decodedLotsCount =
                                Collection.size decodedWorld.lots
                        in
                        Expect.all
                            [ \_ ->
                                decodedLotsCount
                                    |> Expect.equal originalLotsCount
                                    |> Expect.onFail
                                        ("Expected "
                                            ++ String.fromInt originalLotsCount
                                            ++ " lots, got "
                                            ++ String.fromInt decodedLotsCount
                                        )
                            , \_ ->
                                Collection.values decodedWorld.lots
                                    |> List.head
                                    |> Maybe.map .name
                                    |> Expect.equal (Just "LotSchool")
                            ]
                            ()

                    Err error ->
                        Expect.fail ("Decoding failed: " ++ error)
            )
        , test "restored tiles are in Built state"
            (\_ ->
                let
                    originalWorld =
                        Worlds.defaultWorld

                    json =
                        Savegame.encode originalWorld

                    result =
                        Savegame.decode json
                in
                case result of
                    Ok decodedWorld ->
                        let
                            allTilesInCorrectState =
                                Tilemap.foldTiles
                                    (\_ tile acc ->
                                        case tile.kind of
                                            Tile.Fixed _ ->
                                                let
                                                    state =
                                                        FSM.toCurrentState tile.fsm
                                                in
                                                acc && state == Tile.Built

                                            _ ->
                                                acc
                                    )
                                    True
                                    decodedWorld.tilemap
                        in
                        allTilesInCorrectState
                            |> Expect.equal True
                            |> Expect.onFail "Expected all fixed tiles to be in Built state (not transitioning)"

                    Err error ->
                        Expect.fail ("Decoding failed: " ++ error)
            )
        ]


roundTripTests : Test
roundTripTests =
    describe "Round-trip encode/decode"
        [ test "simple world round-trip preserves exact tile IDs"
            (\_ ->
                let
                    originalWorld =
                        Worlds.simpleWorld

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalTileMap =
                                tilemapToTileIdMap originalWorld.tilemap

                            decodedTileMap =
                                tilemapToTileIdMap decodedWorld.tilemap
                        in
                        decodedTileMap
                            |> Expect.equal originalTileMap
                            |> Expect.onFail
                                ("Tile ID mismatch.\nOriginal: "
                                    ++ Debug.toString originalTileMap
                                    ++ "\nDecoded: "
                                    ++ Debug.toString decodedTileMap
                                )

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        , test "default world round-trip preserves exact tile IDs"
            (\_ ->
                let
                    originalWorld =
                        Worlds.defaultWorld

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalTileMap =
                                tilemapToTileIdMap originalWorld.tilemap

                            decodedTileMap =
                                tilemapToTileIdMap decodedWorld.tilemap
                        in
                        decodedTileMap
                            |> Expect.equal originalTileMap

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        , test "world with school round-trip preserves tile IDs and lot data"
            (\_ ->
                let
                    originalWorld =
                        Worlds.worldWithSchool

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalTileMap =
                                tilemapToTileIdMap originalWorld.tilemap

                            decodedTileMap =
                                tilemapToTileIdMap decodedWorld.tilemap

                            originalLots =
                                Collection.values originalWorld.lots

                            decodedLots =
                                Collection.values decodedWorld.lots

                            originalLotNames =
                                List.map .name originalLots |> List.sort

                            decodedLotNames =
                                List.map .name decodedLots |> List.sort
                        in
                        Expect.all
                            [ \_ ->
                                decodedTileMap
                                    |> Expect.equal originalTileMap
                                    |> Expect.onFail "Tile IDs don't match"
                            , \_ ->
                                List.length decodedLots
                                    |> Expect.equal (List.length originalLots)
                                    |> Expect.onFail "Lot count mismatch"
                            , \_ ->
                                decodedLotNames
                                    |> Expect.equal originalLotNames
                                    |> Expect.onFail "Lot names don't match"
                            ]
                            ()

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        , test "world with school round-trip preserves parent tile info"
            (\_ ->
                let
                    originalWorld =
                        Worlds.worldWithSchool

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalParentMap =
                                tilemapToParentTileMap originalWorld.tilemap

                            decodedParentMap =
                                tilemapToParentTileMap decodedWorld.tilemap
                        in
                        decodedParentMap
                            |> Expect.equal originalParentMap
                            |> Expect.onFail
                                ("Parent tile info mismatch.\nOriginal: "
                                    ++ Debug.toString originalParentMap
                                    ++ "\nDecoded: "
                                    ++ Debug.toString decodedParentMap
                                )

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        , test "large world round-trip preserves all tile data"
            (\_ ->
                let
                    originalWorld =
                        Worlds.largeWorld

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalTileMap =
                                tilemapToTileIdMap originalWorld.tilemap

                            decodedTileMap =
                                tilemapToTileIdMap decodedWorld.tilemap

                            originalTileCount =
                                Dict.size originalTileMap

                            decodedTileCount =
                                Dict.size decodedTileMap
                        in
                        Expect.all
                            [ \_ ->
                                decodedTileCount
                                    |> Expect.equal originalTileCount
                                    |> Expect.onFail
                                        ("Expected "
                                            ++ String.fromInt originalTileCount
                                            ++ " tiles, got "
                                            ++ String.fromInt decodedTileCount
                                        )
                            , \_ ->
                                decodedTileMap
                                    |> Expect.equal originalTileMap
                                    |> Expect.onFail "Tile IDs don't match"
                            ]
                            ()

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        , test "large world round-trip preserves road network structure"
            (\_ ->
                let
                    originalWorld =
                        Worlds.largeWorld

                    result =
                        Savegame.encode originalWorld
                            |> Savegame.decode
                            |> Result.map World.updateRoadNetwork
                in
                case result of
                    Ok decodedWorld ->
                        let
                            originalNodeCount =
                                RoadNetwork.size originalWorld.roadNetwork

                            decodedNodeCount =
                                RoadNetwork.size decodedWorld.roadNetwork
                        in
                        decodedNodeCount
                            |> Expect.equal originalNodeCount
                            |> Expect.onFail
                                ("Expected "
                                    ++ String.fromInt originalNodeCount
                                    ++ " road network nodes, got "
                                    ++ String.fromInt decodedNodeCount
                                )

                    Err error ->
                        Expect.fail ("Round-trip failed: " ++ error)
            )
        ]


invalidInputTests : Test
invalidInputTests =
    describe "Invalid input handling"
        [ test "rejects invalid JSON"
            (\_ ->
                let
                    invalidJson =
                        JE.object [ ( "invalid", JE.string "data" ) ]

                    result =
                        Savegame.decode invalidJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Should have rejected invalid JSON"

                    Err _ ->
                        Expect.pass
            )
        , test "rejects wrong version"
            (\_ ->
                let
                    wrongVersionJson =
                        JE.object
                            [ ( "v", JE.int 999 )
                            , ( "seed", JE.list JE.int [ 42, 0 ] )
                            , ( "tmd", JE.list JE.int [ 10, 10 ] )
                            , ( "tilemap", JE.list JE.int [] )
                            , ( "lots", JE.list (JE.list JE.int) [] )
                            ]

                    result =
                        Savegame.decode wrongVersionJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Should have rejected wrong version"

                    Err error ->
                        error
                            |> String.contains "version"
                            |> Expect.equal True
                            |> Expect.onFail ("Expected version error, got: " ++ error)
            )
        , test "rejects tilemap dimensions mismatch"
            (\_ ->
                let
                    mismatchedJson =
                        JE.object
                            [ ( "v", JE.int 1 )
                            , ( "seed", JE.list JE.int [ 42, 0 ] )
                            , ( "tmd", JE.list JE.int [ 5, 5 ] )
                            , ( "tilemap", JE.list JE.int [ 1, 2 ] )
                            , ( "lots", JE.list (JE.list JE.int) [] )
                            ]

                    result =
                        Savegame.decode mismatchedJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Should have rejected tilemap dimensions mismatch"

                    Err error ->
                        error
                            |> String.contains "mismatch"
                            |> Expect.equal True
                            |> Expect.onFail ("Expected dimensions mismatch error, got: " ++ error)
            )
        , test "rejects invalid lot ID"
            (\_ ->
                let
                    invalidLotJson =
                        JE.object
                            [ ( "v", JE.int 1 )
                            , ( "seed", JE.list JE.int [ 42, 0 ] )
                            , ( "tmd", JE.list JE.int [ 10, 10 ] )
                            , ( "tilemap", JE.list JE.int (List.repeat 100 0) )
                            , ( "lots"
                              , JE.list (JE.list JE.int)
                                    [ [ 999, 5, 5 ] ]
                              )
                            ]

                    result =
                        Savegame.decode invalidLotJson
                in
                case result of
                    Ok _ ->
                        Expect.fail "Should have rejected invalid lot ID"

                    Err error ->
                        error
                            |> String.contains "lot"
                            |> Expect.equal True
                            |> Expect.onFail ("Expected lot ID error, got: " ++ error)
            )
        ]


seedDeterminismTests : Test
seedDeterminismTests =
    describe "Seed determinism"
        [ test "encode/decode preserves seed state for deterministic random generation"
            (\_ ->
                let
                    world =
                        Worlds.simpleWorld
                            |> (\w -> { w | seedState = World.seedStateFromInt 12345 })

                    -- Step the seed multiple times to simulate game activity
                    -- This represents the seed state after some game time has passed
                    ( _, seed1 ) =
                        Random.step (Random.int 0 100) world.seedState.currentSeed

                    ( _, seed2 ) =
                        Random.step (Random.int 0 100) seed1

                    ( _, seed3 ) =
                        Random.step (Random.int 0 100) seed2

                    worldWithSteppedSeed =
                        { world
                            | seedState =
                                { initialSeed = world.seedState.initialSeed
                                , currentSeed = seed3
                                , stepCount = world.seedState.stepCount + 3
                                }
                        }

                    encoded =
                        Savegame.encode worldWithSteppedSeed

                    decoded =
                        Savegame.decode encoded
                in
                case decoded of
                    Ok restoredWorld ->
                        let
                            generator =
                                Random.int Random.minInt Random.maxInt

                            ( originalValue1, originalSeed1 ) =
                                Random.step generator seed3

                            ( originalValue2, originalSeed2 ) =
                                Random.step generator originalSeed1

                            ( originalValue3, _ ) =
                                Random.step generator originalSeed2

                            ( restoredValue1, restoredSeed1 ) =
                                Random.step generator restoredWorld.seedState.currentSeed

                            ( restoredValue2, restoredSeed2 ) =
                                Random.step generator restoredSeed1

                            ( restoredValue3, _ ) =
                                Random.step generator restoredSeed2

                            -- All three values should match
                            allMatch =
                                (originalValue1 == restoredValue1)
                                    && (originalValue2 == restoredValue2)
                                    && (originalValue3 == restoredValue3)
                        in
                        Expect.equal allMatch True
                            |> Expect.onFail
                                ("Seed does not continue from savegame - expected sequence ["
                                    ++ String.fromInt originalValue1
                                    ++ ", "
                                    ++ String.fromInt originalValue2
                                    ++ ", "
                                    ++ String.fromInt originalValue3
                                    ++ "] but got ["
                                    ++ String.fromInt restoredValue1
                                    ++ ", "
                                    ++ String.fromInt restoredValue2
                                    ++ ", "
                                    ++ String.fromInt restoredValue3
                                    ++ "]"
                                )

                    Err error ->
                        Expect.fail ("Decode failed: " ++ error)
            )
        ]



--
-- Helpers
--


tilemapToTileIdMap : Tilemap.Tilemap -> Dict ( Int, Int ) Int
tilemapToTileIdMap tilemap =
    Tilemap.foldTiles
        (\cell tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    Dict.insert (Cell.coordinates cell) properties.id acc

                _ ->
                    acc
        )
        Dict.empty
        tilemap


tilemapToParentTileMap : Tilemap.Tilemap -> Dict ( Int, Int ) ( Int, Int )
tilemapToParentTileMap tilemap =
    Tilemap.foldTiles
        (\cell tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    case properties.parentTile of
                        Just ( parentTileId, subgridIndex ) ->
                            Dict.insert (Cell.coordinates cell) ( parentTileId, subgridIndex ) acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        Dict.empty
        tilemap



--
-- Parent tile computation tests
--


parentTileComputationTests : Test
parentTileComputationTests =
    describe "Parent tile computation from lot data"
        [ test "computes parent tiles matching Savegame.encode output"
            (\_ ->
                let
                    world =
                        Worlds.worldWithSchool

                    savedData =
                        Savegame.encode world
                            |> Savegame.decode
                            |> Result.map
                                (\decoded ->
                                    extractParentTilesFromWorld decoded
                                )

                    parentTiles =
                        parentTilesFromLots world
                in
                case savedData of
                    Ok savedParentTiles ->
                        Expect.equal (List.sort savedParentTiles) (List.sort parentTiles)
                            |> Expect.onFail
                                ("Parent tiles don't match.\nExpected (saved): "
                                    ++ Debug.toString (List.sort savedParentTiles)
                                    ++ "\nActual (computed): "
                                    ++ Debug.toString (List.sort parentTiles)
                                )

                    Err error ->
                        Expect.fail ("Failed to encode/decode savegame: " ++ error)
            )
        ]


extractParentTilesFromWorld : World -> List ( Int, TileId, Int )
extractParentTilesFromWorld world =
    Tilemap.foldTiles
        (\cell tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    case properties.parentTile of
                        Just ( parentTileId, subgridIndex ) ->
                            let
                                arrayIndex =
                                    Cell.array1DIndex (Tilemap.getTilemapConfig world.tilemap) cell
                            in
                            ( arrayIndex, parentTileId, subgridIndex ) :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        []
        world.tilemap


parentTilesFromLots : World -> List ( Int, TileId, Int )
parentTilesFromLots world =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig world.tilemap

        -- Build map: cell coords (1-indexed) -> (parentTileId, subgridIndex)
        parentTileMap : Dict CellCoordinates ( TileId, Int )
        parentTileMap =
            Collection.foldl
                (\_ lot acc ->
                    findLotAnchor world lot.id
                        |> Maybe.andThen (\anchorCell -> Lots.findByName lot.name |> Maybe.map (Tuple.pair anchorCell))
                        |> Maybe.andThen (\( anchorCell, newLot ) -> findMatchingLargeTile newLot |> Maybe.map (Tuple.pair anchorCell))
                        |> Maybe.map
                            (\( anchorCell, largeTile ) ->
                                addLotCellsToParentMap tilemapConfig anchorCell largeTile acc
                            )
                        |> Maybe.withDefault acc
                )
                Dict.empty
                world.lots
    in
    -- Convert to list format: (arrayIndex, parentTileId, subgridIndex)
    Dict.toList parentTileMap
        |> List.filterMap
            (\( cellCoords, ( parentTileId, subgridIndex ) ) ->
                Cell.fromCoordinates tilemapConfig cellCoords
                    |> Maybe.map
                        (\cell ->
                            ( Cell.array1DIndex tilemapConfig cell
                            , parentTileId
                            , subgridIndex
                            )
                        )
            )
        |> List.sortBy (\( idx, _, _ ) -> idx)


findLotAnchor : World -> Collection.Id -> Maybe Cell.Cell
findLotAnchor world lotId =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig world.tilemap
    in
    Dict.toList world.lotEntries
        |> List.filterMap
            (\( cellCoords, ( entryLotId, entryDirection ) ) ->
                if Collection.idMatches entryLotId lotId then
                    -- Get entry cell, then find driveway cell (where lot actually starts)
                    Cell.fromCoordinates tilemapConfig cellCoords
                        |> Maybe.andThen (Cell.nextOrthogonalCell tilemapConfig entryDirection)

                else
                    Nothing
            )
        |> List.head


findMatchingLargeTile : NewLot -> Maybe LargeTile
findMatchingLargeTile lot =
    Data.TileSet.lotTiles
        |> List.filterMap
            (\tileConfig ->
                case tileConfig of
                    TileConfig.Large largeTile ->
                        if World.newLotMatchesTile largeTile lot then
                            Just largeTile

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


addLotCellsToParentMap : Tilemap.TilemapConfig -> Cell.Cell -> LargeTile -> Dict CellCoordinates ( TileId, Int ) -> Dict CellCoordinates ( TileId, Int )
addLotCellsToParentMap tilemapConfig drivewayCell largeTile dict =
    let
        subgridConstraints =
            { horizontalCellsAmount = largeTile.width
            , verticalCellsAmount = largeTile.height
            }

        -- Get top-left corner cell of the large tile
        maybeTopLeftCell =
            Tile.largeTileTopLeftCell tilemapConfig drivewayCell largeTile.anchorIndex largeTile
    in
    case maybeTopLeftCell of
        Just topLeftCell ->
            -- For each cell in subgrid
            List.range 0 (largeTile.width * largeTile.height - 1)
                |> List.filterMap
                    (\subgridIndex ->
                        -- Convert subgrid index to cell in subgrid coordinates
                        Cell.fromArray1DIndex subgridConstraints subgridIndex
                            -- Place into global coordinates using Cell.placeIn
                            |> Maybe.andThen (Cell.placeIn tilemapConfig topLeftCell)
                            |> Maybe.map
                                (\globalCell ->
                                    ( Cell.coordinates globalCell
                                    , ( largeTile.id, subgridIndex )
                                    )
                                )
                    )
                |> List.foldl
                    (\( coords, value ) acc -> Dict.insert coords value acc)
                    dict

        Nothing ->
            dict
