module TilemapTests exposing (suite)

import Data.Utility exposing (tenByTenTilemap, tilemapFromCoordinates)
import Data.Worlds
    exposing
        ( highComplexityWorld
        , lowComplexityWorld
        )
import Expect
import Maybe.Extra as Maybe
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell
import Tilemap.Core exposing (Tilemap, cellSupportsRoadPlacement, fixedTileByCell, getTilemapConfig)
import Tilemap.Tile exposing (TileKind(..))


tilemapThatResemblesAIntersection : Tilemap
tilemapThatResemblesAIntersection =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 3, 1 )
        , ( 2, 2 )
        ]


tilemapThatResemblesACurve : Tilemap
tilemapThatResemblesACurve =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 1, 2 )
        ]


suite : Test
suite =
    describe "Tilemap"
        [ describe "bitmasking tile by it's neighbors"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    let
                        tilemap =
                            tilemapThatResemblesAIntersection

                        tilemapConfig =
                            getTilemapConfig tilemap
                    in
                    Cell.fromCoordinates tilemapConfig ( 2, 1 )
                        |> Maybe.andThen (fixedTileByCell tilemap)
                        |> Maybe.map
                            (\tile ->
                                case tile.kind of
                                    Fixed properties ->
                                        properties.id == 14

                                    _ ->
                                        False
                            )
                        |> Maybe.map
                            (Expect.equal True
                                >> Expect.onFail "Expected a T-shaped intersection after the mask is applied."
                            )
                        |> Maybe.withDefault (Expect.fail "Could not find the tile")
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    let
                        tilemap =
                            tilemapThatResemblesACurve

                        tilemapConfig =
                            getTilemapConfig tilemap
                    in
                    Cell.fromCoordinates tilemapConfig ( 1, 1 )
                        |> Maybe.andThen (fixedTileByCell tilemap)
                        |> Maybe.map
                            (\tile ->
                                case tile.kind of
                                    Fixed properties ->
                                        properties.id == 12

                                    _ ->
                                        False
                            )
                        |> Maybe.map
                            (Expect.equal True
                                >> Expect.onFail "Expected a curve road piece after the mask is applied."
                            )
                        |> Maybe.withDefault (Expect.fail "Could not find the tile")
                )
            ]
        , describe ".canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    let
                        tilemap =
                            lowComplexityWorld.tilemap

                        tilemapConfig =
                            getTilemapConfig tilemap
                    in
                    Cell.fromCoordinates tilemapConfig ( 2, 2 )
                        |> Maybe.map (\cell -> cellSupportsRoadPlacement cell lowComplexityWorld.tilemap)
                        |> Maybe.withDefault False
                        |> Expect.equal True
                        |> Expect.onFail "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    let
                        tilemap =
                            highComplexityWorld.tilemap

                        tilemapConfig =
                            getTilemapConfig tilemap
                    in
                    Cell.fromCoordinates tilemapConfig ( 2, 2 )
                        |> Maybe.map (\cell -> cellSupportsRoadPlacement cell tilemap)
                        |> Maybe.withDefault False
                        |> Expect.equal False
                        |> Expect.onFail "Expected invalid world."
                )
            ]
        ]
