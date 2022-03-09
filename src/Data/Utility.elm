module Data.Utility exposing (..)

import Model.Cell as Cell
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World as World exposing (World)
import Simulation.Infrastructure as Infrastructure


tilemapFromCoordinates : List ( Int, Int ) -> Tilemap
tilemapFromCoordinates cellCoordinates =
    let
        cells =
            List.filterMap Cell.fromCoordinates cellCoordinates
    in
    Tilemap.fromCells cells


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty |> Infrastructure.createRoadNetwork tilemap
