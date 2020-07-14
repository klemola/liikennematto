module Board exposing
    ( Board
    , canAddTile
    , connections
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
    getWithIndex coords board
        |> Maybe.map Tuple.second


getWithIndex : Coords -> Board -> Maybe ( Coords, Tile )
getWithIndex coords board =
    Dict.find (\key _ -> key == coords) board


getSafe : Coords -> Board -> Tile
getSafe coords board =
    get coords board
        |> Maybe.withDefault Terrain


set : Coords -> Tile -> Board -> Board
set coords tile board =
    Dict.insert coords tile board


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
            if Tile.connected dir origin destination then
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
        parallelNeighbors =
            Coords.parallelNeighbors coords
                |> List.filterMap (\c -> get c board)

        parallelConnections =
            connections coords tile board

        connects =
            List.length parallelConnections > 0

        hasValidNeighbors =
            parallelNeighbors
                |> List.all (Tile.validNeighbors tile)

        isValid =
            connects && hasValidNeighbors
    in
    Dict.isEmpty board
        || List.isEmpty parallelNeighbors
        || isValid
