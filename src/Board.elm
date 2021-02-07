module Board exposing
    ( Board
    , applyMask
    , defaultTile
    , inBounds
    )

import BitMask
import BoundingBox2d
import Cell exposing (Cell, OrthogonalDirection(..))
import Config exposing (boardSizeScaled)
import Dict exposing (Dict)
import Geometry exposing (LMBoundingBox2d)
import Point2d
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , Orientation(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


type alias Board =
    Dict Cell Tile


exists : Cell -> Board -> Bool
exists cell board =
    case Dict.get cell board of
        Just _ ->
            True

        Nothing ->
            False


boundingBox : LMBoundingBox2d
boundingBox =
    Geometry.boundingBoxWithDimensions boardSizeScaled boardSizeScaled Point2d.origin


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


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
            { north = exists (Cell.next Up origin) board
            , west = exists (Cell.next Left origin) board
            , east = exists (Cell.next Right origin) board
            , south = exists (Cell.next Down origin) board
            }
    in
    parallelTiles
        |> BitMask.fourBitValue
        |> Tile.fromId
