module Board exposing
    ( Board
    , applyMask
    , canBuildRoadAt
    , defaultTile
    , exists
    , get
    , inBounds
    , new
    , remove
    , roadPiecePositions
    , set
    )

import BitMask
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
import Config exposing (boardSizeScaled)
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Direction(..), Orientation(..))
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


type alias Board =
    Dict Cell Tile


new : Board
new =
    Dict.fromList []


get : Cell -> Board -> Maybe Tile
get position board =
    board
        |> Dict.find (\key _ -> key == position)
        |> Maybe.map Tuple.second


exists : Cell -> Board -> Bool
exists position board =
    case get position board of
        Just _ ->
            True

        Nothing ->
            False


inBounds : BoundingBox -> Bool
inBounds bb =
    bb.x >= 0 && bb.x + bb.width <= boardSizeScaled && bb.y >= 0 && bb.y + bb.height <= boardSizeScaled


set : Cell -> Tile -> Board -> Board
set position tile board =
    Dict.insert position tile board


remove : Cell -> Board -> Board
remove position board =
    Dict.remove position board


map : (Cell -> Tile -> Tile) -> Board -> Board
map fn board =
    Dict.map fn board


roadPiecePositions : Board -> List Cell
roadPiecePositions board =
    board
        |> Dict.filter
            (\_ t ->
                Tile.isRoad t
            )
        |> Dict.keys


canBuildRoadAt : Cell -> Board -> Bool
canBuildRoadAt position board =
    let
        xyz l =
            List.length l < 3

        hasLowComplexity corner =
            Cell.cornerAndNeighbors corner position
                |> List.filterMap (\c -> get c board)
                |> xyz
    in
    List.all hasLowComplexity Direction.corners


defaultTile : Tile
defaultTile =
    TwoLaneRoad (Regular Horizontal) Both


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
                |> Maybe.withDefault defaultTile
                |> reApplyModifiersIfNecessary oldTile
        )
        board


chooseTile : Board -> Cell -> Maybe Tile
chooseTile board origin =
    let
        parallelTiles =
            { north = exists (Cell.next origin Up) board
            , west = exists (Cell.next origin Left) board
            , east = exists (Cell.next origin Right) board
            , south = exists (Cell.next origin Down) board
            }
    in
    parallelTiles
        |> BitMask.fourBitValue
        |> Tile.fromId
