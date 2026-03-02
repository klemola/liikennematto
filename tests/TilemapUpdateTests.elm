module TilemapUpdateTests exposing (suite)

import Data.Utility
    exposing
        ( createCell
        , gameModelFromWorld
        , tenByTenTilemap
        )
import Data.Worlds exposing (worldWithSchool)
import Dict
import Expect
import Message exposing (Message(..))
import Model.World as World
import Test exposing (Test, describe, test)
import Tilemap.Core exposing (roadTileFromCell)
import Tilemap.DrivenWFC exposing (DrivenWFC(..), addTileById, restartWfc, runWfc)
import Tilemap.Tile as Tile
import Tilemap.Update
import Tilemap.WFC as WFC


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
        ]
