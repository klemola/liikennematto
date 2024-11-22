module DrivenWFCTests exposing (..)

import Data.Utility
    exposing
        ( createCell
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , testSeed
        )
import Expect
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
                                [ ( 5, 5 ), ( 6, 5 ), ( 7, 5 ), ( 8, 5 ) ]
                                emptyTilemap
                                |> restartWFC testSeed
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 6 5)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        secondHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 7 5)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions [ 6, 20 ]
                        , \_ -> Expect.equal secondHorizontalRoadCellOptions [ 6, 20 ]
                        ]
                        ()
                )
            , test "Should set fixed roads to superposition - vertical roads"
                (\_ ->
                    let
                        tilemap =
                            placeRoadAndUpdateBuffer
                                [ ( 5, 5 ), ( 5, 6 ), ( 5, 7 ), ( 5, 8 ) ]
                                emptyTilemap
                                |> restartWFC testSeed
                                |> WFC.toTilemap

                        firstHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 6)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []

                        secondHorizontalRoadCellOptions =
                            tileByCell tilemap (createCell constraints 5 7)
                                |> Maybe.map tileSuperposition
                                |> Maybe.withDefault []
                    in
                    Expect.all
                        [ \_ -> Expect.equal firstHorizontalRoadCellOptions [ 9, 22, 21 ]
                        , \_ -> Expect.equal secondHorizontalRoadCellOptions [ 9, 22, 21 ]
                        ]
                        ()
                )
            ]
        ]
