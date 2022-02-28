module Data.Utility exposing (..)

import Model.Cell as Cell
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.World as World exposing (World)
import Simulation.Infrastructure as Infrastructure


tilemapFromCoordinates : List ( Int, Int ) -> Tilemap
tilemapFromCoordinates cellCoordinates =
    List.foldl
        (\coords acc ->
            case Cell.fromCoordinates coords of
                Just cell ->
                    -- TODO: check if tile FSM actions can be ignored when creating fixtures
                    Tilemap.addTile cell acc |> Tuple.first

                Nothing ->
                    acc
        )
        Tilemap.empty
        cellCoordinates


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty |> Infrastructure.createRoadNetwork tilemap
