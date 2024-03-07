module Data.Utility exposing
    ( AnchorDef
    , getStartAndEndNode
    , tilemapFromCoordinates
    , worldFromTilemap
    )

import Collection exposing (Id)
import Model.Geometry exposing (OrthogonalDirection)
import Model.World as World exposing (World, createRoadNetwork)
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Tilemap.Cell as Cell
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addAnchor
        , tilemapFromCells
        )


type alias AnchorDef =
    { lotId : Id
    , anchorDirection : OrthogonalDirection
    , cellCoordinates : Cell.CellCoordinates
    }


tilemapConfig : TilemapConfig
tilemapConfig =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


tilemapFromCoordinates : List ( Int, Int ) -> List AnchorDef -> Tilemap
tilemapFromCoordinates cellCoordinates anchorDefs =
    let
        cells =
            List.filterMap (Cell.fromCoordinates tilemapConfig) cellCoordinates

        withCells =
            tilemapFromCells tilemapConfig cells
    in
    addAnchors withCells anchorDefs


addAnchors : Tilemap -> List AnchorDef -> Tilemap
addAnchors tilemap anchorDefs =
    List.foldl
        (\{ cellCoordinates, lotId, anchorDirection } nextTilemap ->
            case Cell.fromCoordinates tilemapConfig cellCoordinates of
                Just cell ->
                    addAnchor cell lotId anchorDirection nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        anchorDefs


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty tilemapConfig |> createRoadNetwork tilemap


getStartAndEndNode : World -> Int -> Int -> Maybe ( RNNodeContext, RNNodeContext )
getStartAndEndNode world startId endId =
    let
        start =
            RoadNetwork.nodeById world.roadNetwork startId

        end =
            RoadNetwork.nodeById world.roadNetwork endId
    in
    Maybe.map2 Tuple.pair start end
