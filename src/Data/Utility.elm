module Data.Utility exposing (..)

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


tilemapFromCoordinates : List ( Int, Int ) -> List AnchorDef -> Tilemap
tilemapFromCoordinates cellCoordinates anchorDefs =
    let
        cells =
            List.filterMap Cell.fromCoordinates cellCoordinates

        withCells =
            Tilemap.fromCells cells
    in
    addAnchors withCells anchorDefs


addAnchors : Tilemap -> List AnchorDef -> Tilemap
addAnchors tilemap anchorDefs =
    List.foldl
        (\{ cellCoordinates, lotId, anchorDirection } nextTilemap ->
            case Cell.fromCoordinates cellCoordinates of
                Just cell ->
                    Tilemap.addAnchor cell lotId anchorDirection nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        anchorDefs


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty |> Infrastructure.createRoadNetwork tilemap
