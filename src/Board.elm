module Board exposing
    ( Board
    , canAddTile
    , canBuildRoadAt
    , connections
    , get
    , getSafe
    , has
    , map
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


has : Coords -> Board -> Bool
has coords board =
    case get coords board of
        Just _ ->
            True

        Nothing ->
            False


set : Coords -> Tile -> Board -> Board
set coords tile board =
    Dict.insert coords tile board


remove : Coords -> Board -> Board
remove coords board =
    Dict.remove coords board


map : (Coords -> Tile -> Tile) -> Board -> Board
map fn board =
    Dict.map fn board


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


canBuildRoadAt : Coords -> Board -> Bool
canBuildRoadAt coords board =
    let
        xyz l =
            List.length l < 3

        hasLowComplexity corner =
            Coords.cornerAndNeighbors corner coords
                |> List.filterMap (\c -> get c board)
                |> xyz
    in
    List.all hasLowComplexity Direction.corners
