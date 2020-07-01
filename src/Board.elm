module Board exposing
    ( Board
    , canAddTile
    , get
    , getSafe
    , new
    , remove
    , roadCoords
    , set
    )

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
    if canAddTile coords tile board then
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


connections : Coords -> Tile -> Board -> List Tile
connections coords origin board =
    let
        validate dir destination =
            if
                Tile.connected dir origin destination
                    && Tile.validNeighbors destination origin
            then
                Just destination

            else
                Nothing

        connection dir =
            get (Coords.next coords dir) board
                |> Maybe.andThen (validate dir)
    in
    Tile.potentialConnections origin
        |> List.filterMap connection


canAddTile : Coords -> Tile -> Board -> Bool
canAddTile coords tile board =
    let
        diagonalNeighborTiles =
            Coords.diagonalNeighbors coords
                |> List.filterMap (\c -> get c board)

        parallelNeighborTiles =
            Coords.parallelNeighbors coords
                |> List.filterMap (\c -> get c board)

        surroundingTiles =
            parallelNeighborTiles ++ diagonalNeighborTiles

        isValidDiagonal anotherTile =
            case anotherTile of
                TwoLaneRoad (Regular _) _ ->
                    True

                TwoLaneRoad (Deadend _) _ ->
                    True

                Terrain ->
                    True

                _ ->
                    False

        doesRoadConnect _ =
            not (List.isEmpty (connections coords tile board))

        isValid _ =
            List.all isValidDiagonal diagonalNeighborTiles && doesRoadConnect ()
    in
    Dict.isEmpty board
        || List.isEmpty surroundingTiles
        || isValid ()
