module Model.Board exposing
    ( Board
    , Tile
    , applyMask
    , boundingBox
    , defaultTile
    , exists
    , inBounds
    , innerLaneOffset
    , isCurve
    , isDeadend
    , isIntersection
    , outerLaneOffset
    , potentialConnections
    , twoLaneRoadHorizontal
    , twoLaneRoadVertical
    )

import BoundingBox2d
import Common
import Config exposing (boardSizeScaledInMeters)
import Dict exposing (Dict)
import Length exposing (Length)
import Model.Cell as Cell exposing (Cell, OrthogonalDirection(..))
import Model.Geometry exposing (LMBoundingBox2d, pixelsToMeters)
import Point2d
import Set exposing (Set)


type alias Board =
    Dict Cell Tile


type alias Tile =
    Int


type alias ParallelNeighbors =
    { north : Bool
    , west : Bool
    , east : Bool
    , south : Bool
    }


innerLaneOffset : Length
innerLaneOffset =
    pixelsToMeters 26


outerLaneOffset : Length
outerLaneOffset =
    pixelsToMeters 54


defaultTile : Tile
defaultTile =
    0


twoLaneRoadHorizontal : Tile
twoLaneRoadHorizontal =
    6


twoLaneRoadVertical : Tile
twoLaneRoadVertical =
    9


deadendTiles : Set Tile
deadendTiles =
    Set.fromList [ 1, 2, 4, 8 ]


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


boundingBox : LMBoundingBox2d
boundingBox =
    Common.boundingBoxWithDimensions boardSizeScaledInMeters boardSizeScaledInMeters Point2d.origin



--
-- Queries
--


isDeadend : Tile -> Bool
isDeadend tile =
    Set.member tile deadendTiles


isCurve : Tile -> Bool
isCurve tile =
    Set.member tile curveTiles


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


exists : Cell -> Board -> Bool
exists cell board =
    Dict.member cell board


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
    fourBitBitmask parallelTiles


{-| Calculates tile number (ID) based on surrounding tiles

    North = 2^0 = 1
    West = 2^1 = 2
    East = 2^2 = 4
    South = 2^3 = 8

    e.g. tile bordered by tiles in north and east directions 1*1 + 2*0 + 4*1 + 8*0 = 0101 = 5

-}
fourBitBitmask : ParallelNeighbors -> Int
fourBitBitmask { north, west, east, south } =
    1 * boolToBinary north + 2 * boolToBinary west + 4 * boolToBinary east + 8 * boolToBinary south


boolToBinary : Bool -> Int
boolToBinary booleanValue =
    if booleanValue then
        1

    else
        0


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
