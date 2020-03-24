module Board exposing (Board, connectedRoads, get, update, view)

import Collage exposing (..)
import Collage.Layout as Layout
import Coords exposing (Coords)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))


type alias Board =
    Dict Coords Tile


get : Coords -> Board -> Tile
get coords board =
    case Dict.find (\key _ -> key == coords) board of
        Just ( _, tile ) ->
            tile

        Nothing ->
            Terrain


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


update : Board -> Board
update board =
    board
        |> withUpdatedTiles


withUpdatedTiles : Board -> Board
withUpdatedTiles board =
    Dict.map (\_ tile -> Tile.update tile) board


view : Float -> List Int -> Board -> Collage msg
view tileSize rg board =
    let
        tile x y =
            get ( x, y ) board
                |> Tile.view tileSize

        col x =
            List.map (tile x) rg
                |> Layout.vertical

        rows =
            List.map col rg
    in
    Layout.horizontal rows
