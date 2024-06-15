module TilemapTests exposing (suite)

import Data.Utility exposing (tilemapFromCoordinates)
import Data.Worlds
    exposing
        ( highComplexityWorld
        , lowComplexityWorld
        )
import Expect
import Lib.Collection exposing (initialId)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Maybe.Extra as Maybe
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell
import Tilemap.Core exposing (Tilemap, canBuildRoadAt, fixedTileByCell, getTilemapConfig)
import Tilemap.Tile exposing (TileKind(..))


tilemapThatResemblesAIntersection : Tilemap
tilemapThatResemblesAIntersection =
    tilemapFromCoordinates
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 3, 1 )
        , ( 2, 2 )
        ]
        []


tilemapThatResemblesACurve : Tilemap
tilemapThatResemblesACurve =
    tilemapFromCoordinates
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 1, 2 )
        ]
        []


tilemapWithAnchor : Tilemap
tilemapWithAnchor =
    tilemapFromCoordinates
        [ ( 1, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        ]
        [ { cellCoordinates = ( 1, 3 )
          , lotId = initialId
          , anchorDirection = Right
          }
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
                                    Fixed tileId ->
                                        tileId == 14

                                    _ ->
                                        False
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a T-shaped intersection after the mask is applied.")
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
                                    Fixed tileId ->
                                        tileId == 12

                                    _ ->
                                        False
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a curve road piece after the mask is applied.")
                )

            -- , test "Creates a lot entry with compatible tiles"
            --     (\_ ->
            --         let
            --             tilemap =
            --                 tilemapWithAnchor
            --             tilemapConfig =
            --                 getTilemapConfig tilemap
            --         in
            --         Cell.fromCoordinates tilemapConfig ( 1, 3 )
            --             |> Maybe.andThen (fixedTileByCell tilemapWithAnchor)
            --             |> Maybe.map
            --                 (\tile ->
            --                     case tile.kind of
            --                         Fixed tileId ->
            --                             tileId == 29
            --                         Superposition _ ->
            --                             False
            --                 )
            --             |> Maybe.unwrap
            --                 (Expect.fail "Could not find the tile")
            --                 (Expect.true "Expected a lot entry road piece after the mask is applied.")
            --     )
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
                        |> Maybe.map (\cell -> canBuildRoadAt cell lowComplexityWorld.tilemap)
                        |> Maybe.withDefault False
                        |> Expect.true "Expected valid world."
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
                        |> Maybe.map (\cell -> canBuildRoadAt cell tilemap)
                        |> Maybe.withDefault False
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
