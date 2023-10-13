module Data.Utility exposing
    ( AnchorDef
    , getStartAndEndNode
    , tilemapFromCoordinates
    , worldFromTilemap
    )

import Collection exposing (Id)
import Data.TileSet exposing (allTiles, defaultTile)
import Model.Cell as Cell
import Model.Geometry exposing (OrthogonalDirection)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World as World exposing (World)
import Random
import Simulation.Infrastructure as Infrastructure


type alias AnchorDef =
    { lotId : Id
    , anchorDirection : OrthogonalDirection
    , cellCoordinates : Cell.CellCoordinates
    }


tilemapConfig : Tilemap.TilemapConfig
tilemapConfig =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    , initialSeed = Random.initialSeed 0
    , defaultTile = defaultTile
    , tiles = allTiles
    }


tilemapFromCoordinates : List ( Int, Int ) -> List AnchorDef -> Tilemap
tilemapFromCoordinates cellCoordinates anchorDefs =
    let
        cells =
            List.filterMap (Cell.fromCoordinates tilemapConfig) cellCoordinates

        withCells =
            Tilemap.fromCells tilemapConfig cells
    in
    addAnchors withCells anchorDefs


addAnchors : Tilemap -> List AnchorDef -> Tilemap
addAnchors tilemap anchorDefs =
    List.foldl
        (\{ cellCoordinates, lotId, anchorDirection } nextTilemap ->
            case Cell.fromCoordinates tilemapConfig cellCoordinates of
                Just cell ->
                    Tilemap.addAnchor cell lotId anchorDirection nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        anchorDefs


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty tilemapConfig |> Infrastructure.createRoadNetwork tilemap


getStartAndEndNode : World -> Int -> Int -> Maybe ( RNNodeContext, RNNodeContext )
getStartAndEndNode world startId endId =
    let
        start =
            RoadNetwork.findNodeByNodeId world.roadNetwork startId

        end =
            RoadNetwork.findNodeByNodeId world.roadNetwork endId
    in
    Maybe.map2 Tuple.pair start end
