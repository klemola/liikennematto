module Board exposing
    ( Board
    , Tile
    , applyMask
    , boundingBox
    , crossIntersection
    , defaultTile
    , inBounds
    , isCurve
    , isDeadend
    , isIntersection
    , potentialConnections
    , twoLaneRoadHorizontal
    , twoLaneRoadVertical
    )

import BitMask
import BoundingBox2d
import Cell exposing (Cell, OrthogonalDirection(..))
import Config exposing (boardSizeScaledInMeters)
import Dict exposing (Dict)
import Geometry exposing (LMBoundingBox2d)
import Point2d
import Set exposing (Set)


type alias Board =
    Dict Cell Tile


type alias Tile =
    Int


exists : Cell -> Board -> Bool
exists cell board =
    Dict.member cell board


twoLaneRoadHorizontal : Tile
twoLaneRoadHorizontal =
    6


twoLaneRoadVertical : Tile
twoLaneRoadVertical =
    9


deadendTiles : Set Tile
deadendTiles =
    Set.fromList [ 1, 2, 4, 8 ]


isDeadend : Tile -> Bool
isDeadend tile =
    Set.member tile deadendTiles


intersectionTiles : Set Tile
intersectionTiles =
    Set.fromList [ 7, 11, 13, 14, crossIntersection ]


crossIntersection : Tile
crossIntersection =
    15


isIntersection : Tile -> Bool
isIntersection tile =
    Set.member tile intersectionTiles


curveTiles : Set Tile
curveTiles =
    Set.fromList [ 3, 5, 10, 12 ]


isCurve : Tile -> Bool
isCurve tile =
    Set.member tile curveTiles


boundingBox : LMBoundingBox2d
boundingBox =
    Geometry.boundingBoxWithDimensions boardSizeScaledInMeters boardSizeScaledInMeters Point2d.origin


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


defaultTile : Tile
defaultTile =
    0


applyMask : Board -> Board
applyMask board =
    Dict.map (\cell _ -> chooseTile board cell) board


chooseTile : Board -> Cell -> Tile
chooseTile board origin =
    let
        parallelTiles =
            { north = exists (Cell.next Up origin) board
            , west = exists (Cell.next Left origin) board
            , east = exists (Cell.next Right origin) board
            , south = exists (Cell.next Down origin) board
            }
    in
    BitMask.fourBitValue parallelTiles


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections tile =
    case tile of
        -- TwoLaneRoad (Regular Vertical)
        9 ->
            [ Up, Down ]

        -- TwoLaneRoad (Regular Horizontal)
        6 ->
            [ Left, Right ]

        -- TwoLaneRoad (Curve TopRight)
        10 ->
            [ Left, Down ]

        -- TwoLaneRoad (Curve TopLeft)
        12 ->
            [ Right, Down ]

        -- TwoLaneRoad (Curve BottomRight)
        3 ->
            [ Left, Up ]

        -- TwoLaneRoad (Curve BottomLeft)
        5 ->
            [ Right, Up ]

        -- TwoLaneRoad (Deadend Up)
        8 ->
            [ Up ]

        -- TwoLaneRoad (Deadend Right)
        2 ->
            [ Right ]

        -- TwoLaneRoad (Deadend Down)
        1 ->
            [ Down ]

        -- TwoLaneRoad (Deadend Left)
        4 ->
            [ Left ]

        -- Intersection _ (T Up)
        7 ->
            Up :: Cell.crossOrthogonalDirection Up

        -- Intersection _ (T Right)
        13 ->
            Right :: Cell.crossOrthogonalDirection Right

        -- Intersection _ (T Down)
        14 ->
            Down :: Cell.crossOrthogonalDirection Down

        -- Intersection _ (T Left)
        11 ->
            Left :: Cell.crossOrthogonalDirection Left

        15 ->
            Cell.allODs

        _ ->
            []
