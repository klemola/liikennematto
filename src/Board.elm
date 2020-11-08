module Board exposing
    ( Board
    , applyMask
    , defaultTile
    , exists
    , inBounds
    , new
    )

import BitMask
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
import Config exposing (boardSizeScaled)
import Dict exposing (Dict)
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


exists : Cell -> Board -> Bool
exists cell board =
    case Dict.get cell board of
        Just _ ->
            True

        Nothing ->
            False


inBounds : BoundingBox -> Bool
inBounds bb =
    bb.x >= 0 && bb.x + bb.width <= boardSizeScaled && bb.y >= 0 && bb.y + bb.height <= boardSizeScaled


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
    Dict.map
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
