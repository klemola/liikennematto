module Model.Board exposing
    ( Board
    , Cell
    , DiagonalDirection(..)
    , OrthogonalDirection(..)
    , Tile
    , addTile
    , boundingBox
    , cellBottomLeftCorner
    , cellBoundingBox
    , cellCenterPoint
    , cellToString
    , cornerCells
    , curveTopLeft
    , diagonalDirections
    , empty
    , exists
    , horizontalRoad
    , inBounds
    , intersectionTDown
    , isCurve
    , isDeadend
    , isIntersection
    , isVerticalDirection
    , nextOrthogonalCell
    , oppositeOrthogonalDirection
    , orthogonalDirectionToLmDirection
    , orthogonalDirections
    , potentialConnections
    , removeTile
    , rowsAndColumnsAmount
    , size
    , tileAt
    , tileSize
    , verticalRoad
    )

import BoundingBox2d
import Common
import Dict exposing (Dict)
import Length exposing (Length)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMDirection2d
        , LMPoint2d
        , down
        , left
        , pixelsToMeters
        , right
        , up
        )
import Point2d
import Quantity
import Set exposing (Set)
import Vector2d


type alias Board =
    Dict Cell Tile


type alias Tile =
    Int


type alias Cell =
    ( Int, Int )


type OrthogonalDirection
    = Up
    | Right
    | Down
    | Left


type DiagonalDirection
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


rowsAndColumnsAmount : Int
rowsAndColumnsAmount =
    10


tileSize : Length
tileSize =
    pixelsToMeters 80


size : Length
size =
    tileSize |> Quantity.multiplyBy (toFloat rowsAndColumnsAmount)


boundingBox : LMBoundingBox2d
boundingBox =
    Common.boundingBoxWithDimensions size size Point2d.origin



--
-- Board construction and modification
--


empty : Board
empty =
    Dict.empty


addTile : Cell -> Board -> Board
addTile cell board =
    board
        |> Dict.insert cell defaultTile
        |> applyMask


removeTile : Cell -> Board -> Board
removeTile cell board =
    board
        |> Dict.remove cell
        |> applyMask



--
-- Bit mask
--


type alias ParallelNeighbors =
    { north : Bool
    , west : Bool
    , east : Bool
    , south : Bool
    }


applyMask : Board -> Board
applyMask board =
    Dict.map (\cell _ -> chooseTile board cell) board


chooseTile : Board -> Cell -> Tile
chooseTile board origin =
    let
        parallelTiles =
            { north = exists (nextOrthogonalCell Up origin) board
            , west = exists (nextOrthogonalCell Left origin) board
            , east = exists (nextOrthogonalCell Right origin) board
            , south = exists (nextOrthogonalCell Down origin) board
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



--
-- Board queries
--


tileAt : Cell -> Board -> Maybe Tile
tileAt cell board =
    Dict.get cell board


inBounds : LMBoundingBox2d -> Bool
inBounds testBB =
    BoundingBox2d.isContainedIn boundingBox testBB


exists : Cell -> Board -> Bool
exists cell board =
    Dict.member cell board



-- Cells


cellBottomLeftCorner : Cell -> LMPoint2d
cellBottomLeftCorner ( cellX, cellY ) =
    let
        ( x, y ) =
            ( toFloat (cellX - 1), toFloat (rowsAndColumnsAmount - cellY) )
    in
    if x < 0 || y < 0 then
        Point2d.origin

    else
        Point2d.xy
            (tileSize |> Quantity.multiplyBy x)
            (tileSize |> Quantity.multiplyBy y)


cellCenterPoint : Cell -> LMPoint2d
cellCenterPoint cell =
    let
        displacement =
            Vector2d.xy
                (Quantity.half tileSize)
                (Quantity.half tileSize)
    in
    cellBottomLeftCorner cell
        |> Point2d.translateBy displacement


cellBoundingBox : Cell -> LMBoundingBox2d
cellBoundingBox cell =
    cellBottomLeftCorner cell
        |> Common.boundingBoxWithDimensions tileSize tileSize


cellToString : Cell -> String
cellToString ( x, y ) =
    "Cell (" ++ String.fromInt x ++ "," ++ String.fromInt y


horizontalOrthogonalDirections : List OrthogonalDirection
horizontalOrthogonalDirections =
    [ Left, Right ]


verticalOrthogonalDirections : List OrthogonalDirection
verticalOrthogonalDirections =
    [ Up, Down ]


orthogonalDirections : List OrthogonalDirection
orthogonalDirections =
    verticalOrthogonalDirections ++ horizontalOrthogonalDirections


diagonalDirections : List DiagonalDirection
diagonalDirections =
    [ TopLeft, TopRight, BottomLeft, BottomRight ]


nextOrthogonalCell : OrthogonalDirection -> Cell -> Cell
nextOrthogonalCell dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


nextDiagonalCell : DiagonalDirection -> Cell -> Cell
nextDiagonalCell c ( x, y ) =
    case c of
        TopLeft ->
            ( x - 1, y - 1 )

        TopRight ->
            ( x + 1, y - 1 )

        BottomLeft ->
            ( x - 1, y + 1 )

        BottomRight ->
            ( x + 1, y + 1 )


oppositeOrthogonalDirection : OrthogonalDirection -> OrthogonalDirection
oppositeOrthogonalDirection dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


crossOrthogonalDirection : OrthogonalDirection -> List OrthogonalDirection
crossOrthogonalDirection fromDir =
    case fromDir of
        Up ->
            horizontalOrthogonalDirections

        Right ->
            verticalOrthogonalDirections

        Down ->
            horizontalOrthogonalDirections

        Left ->
            verticalOrthogonalDirections


isVerticalDirection : OrthogonalDirection -> Bool
isVerticalDirection direction =
    List.member direction verticalOrthogonalDirections


orthogonalDirectionToLmDirection : OrthogonalDirection -> LMDirection2d
orthogonalDirectionToLmDirection dir =
    case dir of
        Up ->
            up

        Right ->
            right

        Down ->
            down

        Left ->
            left


{-| Corner plus natural neighbors (clockwise).

    e.g. Left, TopLeft, Up

-}
cornerCells : DiagonalDirection -> Cell -> List Cell
cornerCells c position =
    case c of
        TopLeft ->
            [ nextOrthogonalCell Left position
            , nextDiagonalCell TopLeft position
            , nextOrthogonalCell Up position
            ]

        TopRight ->
            [ nextOrthogonalCell Up position
            , nextDiagonalCell TopRight position
            , nextOrthogonalCell Right position
            ]

        BottomLeft ->
            [ nextOrthogonalCell Down position
            , nextDiagonalCell BottomLeft position
            , nextOrthogonalCell Left position
            ]

        BottomRight ->
            [ nextOrthogonalCell Right position
            , nextDiagonalCell BottomRight position
            , nextOrthogonalCell Down position
            ]



--
-- Tiles
--


defaultTile : Tile
defaultTile =
    0


horizontalRoad : Tile
horizontalRoad =
    6


verticalRoad : Tile
verticalRoad =
    9


curveBottomRight : Tile
curveBottomRight =
    3


curveBottomLeft : Tile
curveBottomLeft =
    5


curveTopRight : Tile
curveTopRight =
    10


curveTopLeft : Tile
curveTopLeft =
    12


curveTiles : Set Tile
curveTiles =
    Set.fromList
        [ curveTopLeft
        , curveTopRight
        , curveBottomLeft
        , curveBottomRight
        ]


isCurve : Tile -> Bool
isCurve tile =
    Set.member tile curveTiles


deadendDown : Tile
deadendDown =
    1


deadendRight : Tile
deadendRight =
    2


deadendLeft : Tile
deadendLeft =
    4


deadendUp : Tile
deadendUp =
    8


deadendTiles : Set Tile
deadendTiles =
    Set.fromList
        [ deadendDown
        , deadendRight
        , deadendLeft
        , deadendUp
        ]


isDeadend : Tile -> Bool
isDeadend tile =
    Set.member tile deadendTiles


intersectionTUp : Tile
intersectionTUp =
    7


intersectionTLeft : Tile
intersectionTLeft =
    11


intersectionTRight : Tile
intersectionTRight =
    13


intersectionTDown : Tile
intersectionTDown =
    14


intersectionCross : Tile
intersectionCross =
    15


intersectionTiles : Set Tile
intersectionTiles =
    Set.fromList
        [ intersectionTUp
        , intersectionTRight
        , intersectionTDown
        , intersectionTLeft
        , intersectionCross
        ]


isIntersection : Tile -> Bool
isIntersection tile =
    Set.member tile intersectionTiles


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections tile =
    if tile == verticalRoad then
        [ Up, Down ]

    else if tile == horizontalRoad then
        [ Left, Right ]

    else if tile == curveTopRight then
        [ Left, Down ]

    else if tile == curveTopLeft then
        [ Right, Down ]

    else if tile == curveBottomRight then
        [ Left, Up ]

    else if tile == curveBottomLeft then
        [ Right, Up ]

    else if tile == deadendUp then
        [ Up ]

    else if tile == deadendRight then
        [ Right ]

    else if tile == deadendDown then
        [ Down ]

    else if tile == deadendLeft then
        [ Left ]

    else if tile == intersectionTUp then
        Up :: crossOrthogonalDirection Up

    else if tile == intersectionTRight then
        Right :: crossOrthogonalDirection Right

    else if tile == intersectionTDown then
        Down :: crossOrthogonalDirection Down

    else if tile == intersectionTLeft then
        Left :: crossOrthogonalDirection Left

    else if tile == intersectionCross then
        orthogonalDirections

    else
        []
