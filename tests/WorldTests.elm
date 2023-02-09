module WorldTests exposing (suite)

import Collection exposing (initialId)
import Common exposing (boundingBoxWithDimensions)
import Data.Lots exposing (school)
import Data.Worlds
    exposing
        ( worldThatHasAVerticalRoadAtLeftSide
        , worldThatHasParallelRoads
        )
import Expect
import Length
import Model.Cell as Cell
import Model.Lot as Lot
import Model.Tilemap as Tilemap
import Model.World as World
import Point2d
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "World"
        [ describe "World.isEmptyArea"
            [ test "correctly determines if an area is empty (not existings lots)"
                (\_ ->
                    World.isEmptyArea
                        (boundingBoxWithDimensions
                            (Length.meters 32)
                            (Length.meters 32)
                            (Point2d.meters 16 32)
                        )
                        worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.true "Expected the \"world\" to have space."
                )
            , test "correctly determines if an area is empty (existing lot in target area)"
                (\_ ->
                    let
                        tilemapConfig =
                            Tilemap.config worldThatHasAVerticalRoadAtLeftSide.tilemap

                        testLot =
                            Cell.fromCoordinates tilemapConfig ( 1, 8 )
                                |> Maybe.map (Lot.build initialId school)

                        lots =
                            testLot
                                |> Maybe.map (\lot -> Collection.empty |> Collection.addWithId lot.id lot)
                                |> Maybe.withDefault Collection.empty

                        withLot =
                            worldThatHasAVerticalRoadAtLeftSide
                                |> (\world -> { world | lots = lots })
                    in
                    World.isEmptyArea
                        (boundingBoxWithDimensions
                            (Length.meters 32)
                            (Length.meters 32)
                            (Point2d.meters 16 32)
                        )
                        withLot
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "correctly determines if an area is empty (not enough space between roads)"
                (\_ ->
                    World.isEmptyArea
                        (boundingBoxWithDimensions
                            (Length.meters 32)
                            (Length.meters 32)
                            (Point2d.meters 16 32)
                        )
                        worldThatHasParallelRoads
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            , test "reports area as filled if it's out of tilemap bounds"
                (\_ ->
                    World.isEmptyArea
                        (boundingBoxWithDimensions
                            (Length.meters 48)
                            (Length.meters 32)
                            (Point2d.meters 144 16)
                        )
                        worldThatHasAVerticalRoadAtLeftSide
                        |> Expect.false "Expected the \"world\" *not* to have space."
                )
            ]
        ]
