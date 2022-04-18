module Data.Utility exposing (tilemapFromCoordinates, worldFromTilemap)

import Data.Defaults as Defaults
import Model.Cell as Cell
import Model.Entity exposing (Id)
import Model.Geometry exposing (OrthogonalDirection)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World as World exposing (World)
import Simulation.Infrastructure as Infrastructure


type alias AnchorDef =
    { lotId : Id
    , anchorDirection : OrthogonalDirection
    , cellCoordinates : Cell.CellCoordinates
    }


tilemapConfig : Tilemap.TilemapConfig
tilemapConfig =
    { horizontalCellsAmount = Defaults.horizontalCellsAmount
    , verticalCellsAmount = Defaults.verticalCellsAmount
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
