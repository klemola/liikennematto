module Data.Utility exposing
    ( AnchorDef
    , getStartAndEndNode
    , tilemapFromCoordinates
    , worldFromTilemap
    )

import Data.TileSet exposing (tileIdByBitmask)
import Duration
import Lib.Collection exposing (Id)
import Lib.OrthogonalDirection exposing (OrthogonalDirection)
import Model.World as World exposing (World, createRoadNetwork)
import Random
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addAnchor
        , cellBitmask
        , createTilemap
        , getTilemapConfig
        , updateTilemap
        )
import Tilemap.Tile as Tile
import Tilemap.Update exposing (modifyTile)


type alias AnchorDef =
    { lotId : Id
    , anchorDirection : OrthogonalDirection
    , cellCoordinates : Cell.CellCoordinates
    }


tenByTenTilemap : TilemapConfig
tenByTenTilemap =
    { horizontalCellsAmount = 10
    , verticalCellsAmount = 10
    }


tilemapFromCoordinates : List ( Int, Int ) -> List AnchorDef -> Tilemap
tilemapFromCoordinates cellCoordinates anchorDefs =
    let
        cells =
            List.filterMap (Cell.fromCoordinates tenByTenTilemap) cellCoordinates

        withCells =
            tilemapFromCells tenByTenTilemap cells
    in
    addAnchors withCells anchorDefs


addAnchors : Tilemap -> List AnchorDef -> Tilemap
addAnchors tilemap anchorDefs =
    List.foldl
        (\{ cellCoordinates, lotId, anchorDirection } nextTilemap ->
            case Cell.fromCoordinates (getTilemapConfig tilemap) cellCoordinates of
                Just cell ->
                    addAnchor cell lotId anchorDirection nextTilemap

                Nothing ->
                    nextTilemap
        )
        tilemap
        anchorDefs


worldFromTilemap : Tilemap -> World
worldFromTilemap tilemap =
    World.empty tenByTenTilemap |> createRoadNetwork tilemap


tilemapFromCells : TilemapConfig -> List Cell -> Tilemap
tilemapFromCells tilemapConfig cells =
    fromCellsHelper cells (createTilemap tilemapConfig (\_ -> Tile.init Tile.Unintialized))


fromCellsHelper : List Cell -> Tilemap -> Tilemap
fromCellsHelper remainingCells tilemap =
    case remainingCells of
        [] ->
            tilemap

        cell :: others ->
            let
                bitmask =
                    cellBitmask cell tilemap

                ( tilemapWithTile, _ ) =
                    case tileIdByBitmask bitmask of
                        Just tileId ->
                            modifyTile cell
                                tilemap
                                (Random.initialSeed 42)
                                (Tilemap.Core.addTile tileId)

                        Nothing ->
                            ( tilemap, [] )

                tilemapUpdateResult =
                    -- run a FSM update cycle to make sure that tiles are not transitioning
                    updateTilemap (Duration.milliseconds 260) tilemapWithTile
            in
            fromCellsHelper others tilemapUpdateResult.tilemap


getStartAndEndNode : World -> Int -> Int -> Maybe ( RNNodeContext, RNNodeContext )
getStartAndEndNode world startId endId =
    let
        start =
            RoadNetwork.nodeById world.roadNetwork startId

        end =
            RoadNetwork.nodeById world.roadNetwork endId
    in
    Maybe.map2 Tuple.pair start end
