module RoadNetworkDiagnostic exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Data.Worlds as Worlds
import Dict
import Graph
import RoadNetworkOptimized
import Simulation.RoadNetwork
import Tilemap.Core exposing (TileListFilter(..), getTilemapConfig, tilemapToList)
import Tilemap.TileConfig as TileConfig


suite : Benchmark
suite =
    let
        world =
            Worlds.largeWorld

        tilemapConfig =
            getTilemapConfig world.tilemap

        -- Pre-compute data for isolated phase testing
        tilePriority ( _, tile ) =
            case Tilemap.Core.tileToConfig tile of
                Just tileConfig ->
                    TileConfig.graphPriority tileConfig

                Nothing ->
                    TileConfig.maxGraphPriority

        sortedTiles =
            world.tilemap
                |> tilemapToList (\cell tile -> Just ( cell, tile )) NoFilter
                |> List.sortBy tilePriority

        nodesMemoDictOriginal =
            Simulation.RoadNetwork.createConnections
                { tilemap = world.tilemap
                , lotEntries = world.lotEntries
                , nodes = Dict.empty
                , remainingTiles = sortedTiles
                }

        nodesOriginal =
            Dict.values nodesMemoDictOriginal

        ( nodesMemoOptimized, spatialIndex ) =
            RoadNetworkOptimized.createConnectionsWithSpatialIndex
                { tilemap = world.tilemap
                , lotEntries = world.lotEntries
                , nodes = Dict.empty
                , spatialIndex = Dict.empty
                , remainingTiles = sortedTiles
                }

        nodesOptimized =
            Dict.values nodesMemoOptimized

        edgesOriginal =
            Simulation.RoadNetwork.createLanes nodesOriginal

        edgesOptimized =
            RoadNetworkOptimized.createLanes tilemapConfig spatialIndex nodesOptimized

        graphOriginal =
            Graph.fromNodesAndEdges nodesOriginal edgesOriginal

        graphOptimized =
            Graph.fromNodesAndEdges nodesOptimized edgesOptimized
    in
    describe "Road Network Bottleneck Analysis"
        [ describe "Complete pipeline"
            [ benchmark "Original buildRoadNetwork" <|
                \_ -> Simulation.RoadNetwork.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            , benchmark "Optimized buildRoadNetwork" <|
                \_ -> RoadNetworkOptimized.buildRoadNetwork world.tilemap world.lotEntries world.trafficLights
            ]
        , describe "Phase 1: Node creation (should be similar)"
            [ benchmark "Original createConnections" <|
                \_ ->
                    Simulation.RoadNetwork.createConnections
                        { tilemap = world.tilemap
                        , lotEntries = world.lotEntries
                        , nodes = Dict.empty
                        , remainingTiles = sortedTiles
                        }
            , benchmark "Optimized createConnectionsWithSpatialIndex" <|
                \_ ->
                    RoadNetworkOptimized.createConnectionsWithSpatialIndex
                        { tilemap = world.tilemap
                        , lotEntries = world.lotEntries
                        , nodes = Dict.empty
                        , spatialIndex = Dict.empty
                        , remainingTiles = sortedTiles
                        }
            ]
        , describe "Phase 2: Edge creation (THE KEY TEST - should show dramatic difference)"
            [ benchmark "Original createLanes (O(N²))" <|
                \_ -> Simulation.RoadNetwork.createLanes nodesOriginal
            , benchmark "Optimized createLanes (O(N×k))" <|
                \_ -> RoadNetworkOptimized.createLanes tilemapConfig spatialIndex nodesOptimized
            ]
        , describe "Phase 3: Graph construction (should be similar)"
            [ benchmark "Graph.fromNodesAndEdges original" <|
                \_ -> Graph.fromNodesAndEdges nodesOriginal edgesOriginal
            , benchmark "Graph.fromNodesAndEdges optimized" <|
                \_ -> Graph.fromNodesAndEdges nodesOptimized edgesOptimized
            ]
        , describe "Phase 4: Traffic control (should be similar)"
            [ benchmark "Original setupTrafficControl" <|
                \_ -> Simulation.RoadNetwork.setupTrafficControl world.trafficLights graphOriginal
            , benchmark "Optimized setupTrafficControl" <|
                \_ -> RoadNetworkOptimized.setupTrafficControl world.trafficLights graphOptimized
            ]
        ]


main : BenchmarkProgram
main =
    program suite
