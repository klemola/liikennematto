module DrivenWFCTests exposing (suite)

import Data.Utility
    exposing
        ( createCell
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        , tilemapFromCoordinates
        )
import Expect
import Maybe.Extra as Maybe
import Set
import Test exposing (..)
import Tilemap.Cell as Cell
import Tilemap.Core exposing (Tilemap, createTilemap, tileByCell)
import Tilemap.DrivenWFC exposing (restartWFC)
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


suite : Test
suite =
    describe "Tilemap.DrivenWFC"
        [ describe ".restartWFC"
            [ test "Should set fixed roads to superposition - horizontal roads"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ), ( 9, 5 ), ( 10, 5 ) ]
                                emptyTilemap
                                |> restartWFC testSeed Set.empty
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
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions [ 6, 20 ]
                        , \_ -> Expect.equal secondHorizontalRoadCellOptions [ 6, 20 ]
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
                                |> restartWFC testSeed Set.empty
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
                        , \_ -> Expect.equal lastHorizontalRoadCellOptions [ 9, 22, 21 ]
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
                                |> restartWFC testSeed Set.empty
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
                        , \_ -> Expect.equal lastHorizontalRoadCellOptions [ 9, 21 ]
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
                                []
                                |> restartWFC testSeed Set.empty
                                |> WFC.toTilemap
                    in
                    Expect.all
                        [ \_ ->
                            Expect.true "tile remains fixed"
                                (tileByCell tilemap (createCell constraints 5 6)
                                    |> Maybe.unwrap False Tile.isFixed
                                )
                        , \_ ->
                            Expect.true "tile remains fixed"
                                (tileByCell tilemap (createCell constraints 5 7)
                                    |> Maybe.unwrap False Tile.isFixed
                                )
                        ]
                        ()
                )
            ]
        ]
