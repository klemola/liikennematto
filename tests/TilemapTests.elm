module TilemapTests exposing (suite)

import Data.Utility exposing (tilemapFromCoordinates)
import Data.Worlds
    exposing
        ( highComplexityWorld
        , lowComplexityWorld
        )
import Expect
import Maybe.Extra as Maybe
import Model.Cell as Cell
import Model.Geometry exposing (OrthogonalDirection(..))
import Model.Tilemap as Tilemap exposing (Tilemap)
import Test exposing (Test, describe, test)


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
          , lotId = 1
          , anchorDirection = Right
          }
        ]


suite : Test
suite =
    describe "Tilemap"
        [ describe "bitmasking tile by it's neighbors"
            [ test "Creates an intersection with compatible tiles"
                (\_ ->
                    Cell.fromCoordinates ( 2, 1 )
                        |> Maybe.andThen (Tilemap.tileAt tilemapThatResemblesAIntersection)
                        |> Maybe.map
                            (\tile ->
                                tile.kind == 14
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a T-shaped intersection after the mask is applied.")
                )
            , test "Creates a curve with compatible tiles"
                (\_ ->
                    Cell.fromCoordinates ( 1, 1 )
                        |> Maybe.andThen (Tilemap.tileAt tilemapThatResemblesACurve)
                        |> Maybe.map
                            (\tile ->
                                tile.kind == 12
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a curve road piece after the mask is applied.")
                )
            , test "Creates a lot entry with compatible tiles"
                (\_ ->
                    Cell.fromCoordinates ( 1, 3 )
                        |> Maybe.andThen (Tilemap.tileAt tilemapWithAnchor)
                        |> Maybe.map
                            (\tile ->
                                tile.kind == 29
                            )
                        |> Maybe.unwrap
                            (Expect.fail "Could not find the tile")
                            (Expect.true "Expected a lot entry road piece after the mask is applied.")
                )
            ]
        , describe ".canBuildRoadAt"
            [ test "Allows a low complexity setup"
                (\_ ->
                    Cell.fromCoordinates ( 2, 2 )
                        |> Maybe.map (\cell -> Tilemap.canBuildRoadAt cell lowComplexityWorld.tilemap)
                        |> Maybe.withDefault False
                        |> Expect.true "Expected valid world."
                )
            , test "Disallows a complex setup"
                (\_ ->
                    Cell.fromCoordinates ( 2, 2 )
                        |> Maybe.map (\cell -> Tilemap.canBuildRoadAt cell highComplexityWorld.tilemap)
                        |> Maybe.withDefault False
                        |> Expect.false "Expected invalid world."
                )
            ]
        ]
