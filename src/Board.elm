module Board exposing (Board, connectedRoads, get, getSafe, set, view)

import Collage exposing (..)
import Config exposing (boardSize, tileSize)
import Coords exposing (Coords)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Graphics
import Tile exposing (Tile(..))


type alias Board =
    Dict Coords Tile


get : Coords -> Board -> Maybe Tile
get coords board =
    Dict.find (\key _ -> key == coords) board
        |> Maybe.map Tuple.second


getSafe : Coords -> Board -> Tile
getSafe coords board =
    get coords board
        |> Maybe.withDefault Terrain


set : Coords -> Tile -> Board -> Board
set coords tile board =
    Dict.insert coords tile board


connectedTiles : Board -> Coords -> List ( Coords, Tile )
connectedTiles board coords =
    let
        neighborCoords =
            Coords.neighbors coords

        pickNeighbors crds tile =
            if List.member crds neighborCoords then
                Just tile

            else
                Nothing
    in
    board
        |> Dict.filterMap pickNeighbors
        |> Dict.toList


connectedRoads : Board -> Coords -> List ( Coords, Tile )
connectedRoads board coords =
    connectedTiles board coords
        |> List.filter (\( c, t ) -> Tile.isRoad t)


view : Board -> Collage msg
view board =
    let
        drawTile x y =
            getSafe ( x, y ) board
                |> Tile.view tileSize
    in
    Graphics.grid boardSize drawTile
