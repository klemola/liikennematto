module Board exposing
    ( Board
    , applyMask
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

import BitMask
import Config
import Coords exposing (Coords)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..))
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


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


applyMask : Board -> Board
applyMask board =
    let
        -- applies modifiers (traffic direction, intersection control type) if new tile is compatible
        -- Room for improvement: if Tile shape is decoupled from modifiers, this step is unnecessary
        reApplyModifiersIfNecessary oldTile newTile =
            case ( oldTile, newTile ) of
                ( TwoLaneRoad _ OneWay, _ ) ->
                    Tile.toggleTrafficDirection newTile

                -- signal control can't be restored on a T intersection
                ( Intersection (Signal _) Crossroads, Intersection _ (T dir) ) ->
                    Tile.defaultIntersectionControl (T dir)
                        |> Tile.setIntersectionControl newTile

                -- otherwise intersection shape is compatible (e.g. from T to Crossroads)
                ( Intersection control _, _ ) ->
                    Tile.setIntersectionControl newTile control

                _ ->
                    newTile
    in
    map
        (\coords oldTile ->
            chooseTile board coords
                |> Maybe.withDefault Config.defaultTile
                |> reApplyModifiersIfNecessary oldTile
        )
        board


chooseTile : Board -> Coords -> Maybe Tile
chooseTile board origin =
    let
        parallelTiles =
            { north = has (Coords.next origin Up) board
            , west = has (Coords.next origin Left) board
            , east = has (Coords.next origin Right) board
            , south = has (Coords.next origin Down) board
            }
    in
    parallelTiles
        |> BitMask.fourBitValue
        |> Tile.fromId
