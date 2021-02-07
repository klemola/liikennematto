module Lot exposing
    ( Anchor
    , Building
    , BuildingKind(..)
    , Lot
    , NewLot
    , all
    , anchorCell
    , bottomLeftCorner
    , boundingBox
    , fromNewLot
    , inBounds
    , parkingSpot
    )

import Cell exposing (Cell, OrthogonalDirection(..))
import Collision exposing (BoundingBox)
import Config exposing (tileSize)
import Geometry exposing (LMPoint2d)


type alias Lot =
    { content : Building
    , width : Float
    , height : Float
    , position : LMPoint2d
    , anchor : Anchor
    }


type alias NewLot =
    { content : Building
    , width : Float
    , height : Float
    }


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    ( Cell, OrthogonalDirection )


type alias Building =
    { kind : BuildingKind
    , entryDirection : OrthogonalDirection
    }


type BuildingKind
    = ResidentialA
    | ResidentialB
    | ResidentialC
    | ResidentialD
    | ResidentialE
    | TwoByOneTest
    | ThreeByThreeTest
    | TwoByThreeTest


all : List NewLot
all =
    [ { content = Building ResidentialA Down, width = tileSize, height = tileSize }
    , { content = Building ResidentialB Right, width = tileSize, height = tileSize }
    , { content = Building ResidentialC Left, width = tileSize, height = tileSize }
    , { content = Building ResidentialD Up, width = tileSize, height = tileSize }
    , { content = Building ResidentialE Down, width = 2 * tileSize, height = 2 * tileSize }
    , { content = Building TwoByOneTest Left, width = 2 * tileSize, height = tileSize }
    , { content = Building ThreeByThreeTest Right, width = 3 * tileSize, height = 3 * tileSize }
    , { content = Building TwoByThreeTest Up, width = 2 * tileSize, height = 3 * tileSize }
    ]


fromNewLot : ( NewLot, Cell ) -> Lot
fromNewLot ( newLot, aCell ) =
    let
        anchor =
            ( aCell, Cell.oppositeOrthogonalDirection newLot.content.entryDirection )

        position =
            bottomLeftCorner newLot anchor
    in
    { content = newLot.content
    , width = newLot.width
    , height = newLot.height
    , position = position
    , anchor = anchor
    }


bottomLeftCorner : NewLot -> Anchor -> LMPoint2d
bottomLeftCorner { width, height } ( aCell, aDir ) =
    let
        origin =
            aCell
                |> Cell.next aDir
                |> Cell.bottomLeftCorner

        adjustedForVerticalEntry =
            origin
                |> Geometry.translatePointBy (width - tileSize) (height - tileSize)
    in
    case aDir of
        Down ->
            adjustedForVerticalEntry

        Left ->
            adjustedForVerticalEntry

        _ ->
            origin


parkingSpot : Lot -> LMPoint2d
parkingSpot lot =
    let
        origin =
            Cell.bottomLeftCorner (entryCell lot)

        ( shiftX, shiftY ) =
            case lot.content.entryDirection of
                Up ->
                    ( tileSize / 2, tileSize )

                Right ->
                    ( tileSize, tileSize / 2 )

                Down ->
                    ( tileSize / 2, 0 )

                Left ->
                    ( 0, tileSize / 2 )
    in
    origin
        |> Geometry.translatePointBy shiftX shiftY


entryCell : Lot -> Cell
entryCell lot =
    let
        ( aCell, aDir ) =
            lot.anchor
    in
    Cell.next aDir aCell


anchorCell : Lot -> Cell
anchorCell lot =
    Tuple.first lot.anchor


boundingBox : Lot -> BoundingBox
boundingBox lot =
    let
        { x, y } =
            Geometry.pointToPosition lot.position
    in
    { x = x, y = y, width = lot.width, height = lot.height }


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    Collision.aabb (Cell.boundingBox cell) (boundingBox lot)
