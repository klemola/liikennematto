module TilemapUpdateTests exposing (suite)

import Data.Utility
    exposing
        ( createCell
        , gameModelFromWorld
        , placeRoad
        , tenByTenTilemap
        , testSeed
        )
import Data.Worlds exposing (worldWithSchool)
import Dict
import Duration
import Expect
import Message exposing (Message(..))
import Model.World as World
import Test exposing (Test, describe, test)
import Tilemap.Core exposing (isDestructivePlacement, roadTileFromCell)
import Tilemap.DrivenWFC exposing (DrivenWFC(..), addTileById, initDrivenWfc, restartWfc, runWfc)
import Tilemap.Tile as Tile
import Tilemap.Update
import Tilemap.WFC as WFC
import Time


suite : Test
suite =
    describe "Tilemap.Update"
        [ describe "WFCChunkProcessed"
            [ test "Stale WFCSolved from a previous run should not overwrite player-built roads"
                (\_ ->
                    let
                        world =
                            worldWithSchool

                        model =
                            gameModelFromWorld world

                        -- Run N
                        wfcN =
                            restartWfc world.seedState Dict.empty world.tilemap

                        staleResult =
                            runWfc 0 world.tilemap (WFC.solve wfcN)

                        newRoadCell =
                            createCell tenByTenTilemap 9 5

                        -- deadendRight
                        newRoadId =
                            5

                        ( tilemapWithNewRoad, _ ) =
                            addTileById world.seedState Dict.empty newRoadCell newRoadId world.tilemap

                        modelWithNewRoad =
                            { model | world = World.setTilemap tilemapWithNewRoad model.world }

                        -- Run N+1
                        wfcN1 =
                            restartWfc world.seedState Dict.empty tilemapWithNewRoad

                        modelWithActiveWfc =
                            { modelWithNewRoad | wfc = WFCActive 1 wfcN1 }

                        -- If WFC.solve failed, staleResult contains WFCFailed, and the handler
                        -- would not overwrite the tilemap — making the test pass with unintenional results
                        ( _, staleDrivenWfc, _ ) =
                            staleResult

                        staleResultIsWFCSolved =
                            case staleDrivenWfc of
                                WFCSolved _ _ _ _ ->
                                    True

                                _ ->
                                    False

                        ( finalModel, _ ) =
                            Tilemap.Update.update (WFCChunkProcessed staleResult) modelWithActiveWfc
                    in
                    if not staleResultIsWFCSolved then
                        Expect.fail "Precondition failed: WFC.solve did not produce WFCSolved. Test setup is invalid."

                    else
                        roadTileFromCell newRoadCell finalModel.world.tilemap
                            |> Maybe.andThen Tile.id
                            |> Maybe.withDefault 0
                            |> Expect.equal newRoadId
                )
            ]
        , describe "isDestructivePlacement"
            -- worldWithSchool: vertical road on column 5, a 3x3 school lot whose
            -- entry is on (5, 4) and whose driveway (entryDirection Right) is (6, 4).
            [ test "is True on a lot subtile cell (the driveway)"
                (\_ ->
                    isDestructivePlacement (createCell tenByTenTilemap 6 4) worldWithSchool.tilemap
                        |> Expect.equal True
                )
            , test "is True on the lot entry cell (erasing it removes the whole lot)"
                (\_ ->
                    isDestructivePlacement (createCell tenByTenTilemap 5 4) worldWithSchool.tilemap
                        |> Expect.equal True
                )
            , test "is False on an empty cell"
                (\_ ->
                    isDestructivePlacement (createCell tenByTenTilemap 1 1) worldWithSchool.tilemap
                        |> Expect.equal False
                )
            , test "is False on a plain road cell that is not a lot entry"
                (\_ ->
                    isDestructivePlacement (createCell tenByTenTilemap 5 1) worldWithSchool.tilemap
                        |> Expect.equal False
                )
            ]
        , describe "describeDestructiveTarget"
            [ test "names the lot when placing on a lot subtile"
                (\_ ->
                    World.describeDestructiveTarget (createCell tenByTenTilemap 6 4) worldWithSchool
                        |> Expect.equal (Just World.DestructiveLot)
                )
            , test "names the lot when placing on its entry cell"
                (\_ ->
                    World.describeDestructiveTarget (createCell tenByTenTilemap 5 4) worldWithSchool
                        |> Expect.equal (Just World.DestructiveLot)
                )
            , test "is Nothing on a non-destructive cell"
                (\_ ->
                    World.describeDestructiveTarget (createCell tenByTenTilemap 1 1) worldWithSchool
                        |> Expect.equal Nothing
                )
            ]
        , describe "CheckQueues"
            [ test "Stays Pending when build history has fewer than 3 cells, regardless of elapsed time"
                (\_ ->
                    let
                        emptyWorld =
                            World.empty testSeed tenByTenTilemap

                        baseModel =
                            gameModelFromWorld emptyWorld

                        tilemapWithTwoRoads =
                            placeRoad [ ( 5, 5 ), ( 6, 5 ) ] emptyWorld.tilemap

                        modelWithTwoRoads =
                            { baseModel
                                | world = World.setTilemap tilemapWithTwoRoads baseModel.world
                                , wfc = initDrivenWfc (Time.millisToPosix 0)
                            }

                        -- 3000ms elapsed, well past the old 2s waitedEnough threshold.
                        ( resultModel, _ ) =
                            Tilemap.Update.update
                                (CheckQueues (Time.millisToPosix 3000) (Duration.milliseconds 3000))
                                modelWithTwoRoads
                    in
                    case resultModel.wfc of
                        WFCPending _ _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected WFC to remain Pending with only 2 placements"
                )
            ]
        ]
