module Lot exposing
    ( Anchor
    , Building
    , BuildingKind(..)
    , Lot
    , NewLot
    , all
    , anchorCell
    , anchorTo
    , bottomLeftCorner
    , boundingBox
    , entryCell
    , inBounds
    , resident
    )

import Car exposing (CarKind(..))
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
import Config exposing (tileSize)
import Direction exposing (Direction(..))
import Position exposing (Position)


type alias Lot =
    { content : Building
    , width : Float
    , height : Float
    , position : Position
    , anchor : Anchor
    }


type alias NewLot =
    { content : Building
    , width : Float
    , height : Float
    }


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    ( Cell, Direction )


type alias Building =
    { kind : BuildingKind
    , entryDirection : Direction
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
    , { content = Building ResidentialE Down, width = 2 * tileSize, height = 2 * tileSize }
    , { content = Building TwoByOneTest Left, width = 2 * tileSize, height = tileSize }
    , { content = Building ThreeByThreeTest Right, width = 3 * tileSize, height = 3 * tileSize }
    , { content = Building TwoByThreeTest Up, width = 2 * tileSize, height = 3 * tileSize }
    , { content = Building ResidentialD Up, width = tileSize, height = tileSize }
    ]


anchorTo : NewLot -> ( Cell, a ) -> Lot
anchorTo newLot ( aCell, _ ) =
    let
        anchor =
            ( aCell, Direction.opposite newLot.content.entryDirection )

        position =
            bottomLeftCorner newLot anchor
    in
    { content = newLot.content
    , width = newLot.width
    , height = newLot.height
    , position = position
    , anchor = anchor
    }


bottomLeftCorner : NewLot -> Anchor -> Position
bottomLeftCorner { width, height } ( aCell, aDir ) =
    let
        origin =
            -- entry cell (inside lot)
            Cell.next aCell aDir
                |> Cell.bottomLeftCorner
    in
    case aDir of
        Up ->
            origin

        Right ->
            origin

        Down ->
            ( Tuple.first origin - (width - tileSize)
            , Tuple.second origin - (height - tileSize)
            )

        Left ->
            ( Tuple.first origin - (width - tileSize)
            , Tuple.second origin - (height - tileSize)
            )


entryCell : Lot -> Cell
entryCell lot =
    let
        ( aCell, aDir ) =
            lot.anchor
    in
    Cell.next aCell aDir


anchorCell : Lot -> Cell
anchorCell lot =
    Tuple.first lot.anchor


boundingBox : Lot -> BoundingBox
boundingBox lot =
    let
        ( x, y ) =
            lot.position
    in
    { x = x, y = y, width = lot.width, height = lot.height }


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    Collision.aabb (Cell.boundingBox cell) (boundingBox lot)


resident : Lot -> Maybe CarKind
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just SedanA

        ResidentialB ->
            Just SedanB

        ResidentialC ->
            Just SedanC

        ResidentialD ->
            Just SedanD

        ResidentialE ->
            Just SedanE

        _ ->
            Nothing
