module Board exposing
    ( Board
    , applyMask
    , canBuildRoadAt
    , exists
    , get
    , inBounds
    , new
    , remove
    , roadPosition
    , set
    )

import BitMask
import Config
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..), Orientation(..))
import Position exposing (Position)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , Tile(..)
        , TrafficDirection(..)
        )


type alias Board =
    Dict Position Tile


new : Board
new =
    Dict.fromList []


get : Position -> Board -> Maybe Tile
get position board =
    getWithIndex position board
        |> Maybe.map Tuple.second


getWithIndex : Position -> Board -> Maybe ( Position, Tile )
getWithIndex position board =
    Dict.find (\key _ -> key == position) board


exists : Position -> Board -> Bool
exists position board =
    case get position board of
        Just _ ->
            True

        Nothing ->
            False


inBounds : Position -> Bool
inBounds ( x, y ) =
    let
        maxSize =
            toFloat Config.boardSize
    in
    x > 0 && x <= maxSize && y > 0 && y <= maxSize


set : Position -> Tile -> Board -> Board
set position tile board =
    Dict.insert position tile board


remove : Position -> Board -> Board
remove position board =
    Dict.remove position board


map : (Position -> Tile -> Tile) -> Board -> Board
map fn board =
    Dict.map fn board


roadPosition : Board -> List Position
roadPosition board =
    board
        |> Dict.filter
            (\_ t ->
                Tile.isRoad t
            )
        |> Dict.keys


canBuildRoadAt : Position -> Board -> Bool
canBuildRoadAt position board =
    let
        xyz l =
            List.length l < 3

        hasLowComplexity corner =
            Position.cornerAndNeighbors corner position
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
        (\position oldTile ->
            chooseTile board position
                |> Maybe.withDefault Config.defaultTile
                |> reApplyModifiersIfNecessary oldTile
        )
        board


chooseTile : Board -> Position -> Maybe Tile
chooseTile board origin =
    let
        parallelTiles =
            { north = exists (Position.next origin Up) board
            , west = exists (Position.next origin Left) board
            , east = exists (Position.next origin Right) board
            , south = exists (Position.next origin Down) board
            }
    in
    parallelTiles
        |> BitMask.fourBitValue
        |> Tile.fromId
