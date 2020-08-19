module Board exposing
    ( Board
    , canBuildRoadAt
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
