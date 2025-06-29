module DrivenWFCTests exposing (suite)

import Data.Utility
    exposing
        ( createCell
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        , tilemapFromCoordinates
        )
import Data.Worlds exposing (worldWithSchool)
import Dict
import Expect
import Lib.FSM as FSM
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Maybe.Extra as Maybe
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core exposing (Tilemap, createTilemap, fixedTileByCell, tileByCell, tileNeighborIn)
import Tilemap.DrivenWFC exposing (addTileById, onRemoveTile, restartWfc)
import Tilemap.Tile as Tile exposing (Tile)
import Tilemap.TileConfig exposing (TileId)
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
                            2

                        ( tilemapWithTile, _ ) =
                            addTileById testSeed Dict.empty newCell deadendRightId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendRightId
                        , neighborCellHasTile newCell OrthogonalDirection.Up Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Right Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Down Tile.Unintialized
                        , neighborCellHasTile newCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 6 "RoadHorizontal"))
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
                            15

                        ( tilemapWithTile, _ ) =
                            addTileById testSeed Dict.empty newCell intersectionCrossId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell intersectionCrossId
                        , neighborCellHasTile newCell OrthogonalDirection.Up (Tile.Fixed (fixedTileProps 8 "RoadDeadendUp"))
                        , neighborCellHasTile newCell OrthogonalDirection.Right (Tile.Fixed (fixedTileProps 6 "RoadHorizontal"))
                        , neighborCellHasTile newCell OrthogonalDirection.Down (Tile.Fixed (fixedTileProps 1 "RoadDeadendDown"))
                        , neighborCellHasTile newCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 6 "RoadHorizontal"))
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
                            4

                        ( tilemapWithTile, _ ) =
                            addTileById testSeed Dict.empty newCell deadendLeftId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendLeftId
                        , neighborCellHasTile newCell
                            OrthogonalDirection.Right
                            (Tile.Fixed (fixedTileProps 64 "RoadIntersectionTLeftLotEntryRight"))
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
                            1

                        ( tilemapWithTile, _ ) =
                            addTileById testSeed Dict.empty newCell deadendDownId tilemap
                    in
                    Expect.all
                        [ cellHasTile newCell deadendDownId
                        , neighborCellHasTile newCell
                            OrthogonalDirection.Up
                            (Tile.Fixed (fixedTileProps 14 "RoadIntersectionTDown"))
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
                            onRemoveTile testSeed Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell OrthogonalDirection.Up Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Right Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Down Tile.Unintialized
                        , neighborCellHasTile removedCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 2 "RoadDeadendRight"))
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
                            onRemoveTile testSeed Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell OrthogonalDirection.Up (Tile.Fixed (fixedTileProps 1 "RoadDeadendDown"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Right (Tile.Fixed (fixedTileProps 4 "RoadDeadendLeft"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Down (Tile.Fixed (fixedTileProps 8 "RoadDeadendUp"))
                        , neighborCellHasTile removedCell OrthogonalDirection.Left (Tile.Fixed (fixedTileProps 2 "RoadDeadendRight"))
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
                            onRemoveTile testSeed Dict.empty removedCell tilemap

                        tilemapWithoutTile =
                            WFC.toTilemap wfcModel
                    in
                    Expect.all
                        [ cellTileIsBeingRemoved removedCell
                        , neighborCellHasTile removedCell
                            OrthogonalDirection.Up
                            (Tile.Fixed (fixedTileProps 58 "RoadDeadendDownLotEntryRight"))
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
                                |> restartWfc testSeed Dict.empty
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
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions [ 6, 50 ]
                        , \_ -> Expect.equal secondHorizontalRoadCellOptions [ 6, 50 ]
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
                                |> restartWfc testSeed Dict.empty
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 6)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        lastHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 9)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions []
                        , \_ -> Expect.equal lastHorizontalRoadCellOptions [ 9, 52, 51 ]
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
                                |> restartWfc testSeed Dict.empty
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 2 6)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        lastHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 2 9)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions []
                        , \_ -> Expect.equal lastHorizontalRoadCellOptions [ 9, 51 ]
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
                                |> restartWfc testSeed Dict.empty
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
        ]
