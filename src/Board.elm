module Board exposing (Board, canAddTile, get, getSafe, new, remove, roadCoords, set)

import Coords exposing (Coords)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Tile exposing (RoadKind(..), Tile(..))


type alias Board =
    Dict Coords Tile


new : Board
new =
    Dict.fromList []


get : Board -> Coords -> Maybe Tile
get board coords =
    Dict.find (\key _ -> key == coords) board
        |> Maybe.map Tuple.second


getSafe : Board -> Coords -> Tile
getSafe board coords =
    get board coords
        |> Maybe.withDefault Terrain


set : Board -> Coords -> Tile -> Board
set board coords tile =
    if canAddTile board coords tile then
        Dict.insert coords tile board

    else
        board


remove : Coords -> Board -> Board
remove coords board =
    Dict.remove coords board


roadCoords : Board -> List Coords
roadCoords board =
    board
        |> Dict.filter
            (\_ t ->
                Tile.isRoad t
            )
        |> Dict.keys


connections : Board -> Coords -> Tile -> List Tile
connections board coords origin =
    let
        validate dir tile =
            if
                List.member (Direction.opposite dir) (Tile.potentialConnections tile)
                    && Tile.validNeighbors tile origin
            then
                Just tile

            else
                Nothing

        connection dir =
            get board (Coords.next coords dir)
                |> Maybe.andThen (validate dir)
    in
    Tile.potentialConnections origin
        |> List.filterMap connection


canAddTile : Board -> Coords -> Tile -> Bool
canAddTile board coords tile =
    let
        diagonalNeighborTiles =
            Coords.diagonalNeighbors coords
                |> List.filterMap (get board)

        parallelNeighborTiles =
            Coords.parallelNeighbors coords
                |> List.filterMap (get board)

        surroundingTiles =
            parallelNeighborTiles ++ diagonalNeighborTiles

        isValidDiagonal anotherTile =
            case anotherTile of
                TwoLaneRoad (Regular _) ->
                    True

                TwoLaneRoad (Deadend _) ->
                    True

                Terrain ->
                    True

                _ ->
                    False

        doesRoadConnect _ =
            not (List.isEmpty (connections board coords tile))

        isValid _ =
            List.all isValidDiagonal diagonalNeighborTiles && doesRoadConnect ()
    in
    Dict.isEmpty board
        || List.isEmpty surroundingTiles
        || isValid ()
