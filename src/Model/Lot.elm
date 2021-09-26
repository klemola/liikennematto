module Model.Lot exposing
    ( Anchor
    , Building
    , BuildingKind(..)
    , Lot
    , Lots
    , NewLot
    , allLots
    , anchorCell
    , bottomLeftCorner
    , boundingBox
    , entryDetails
    , fromNewLot
    , inBounds
    , parkingSpotOrientation
    , resident
    )

import Angle exposing (Angle)
import BoundingBox2d
import Color
import Common
import Dict exposing (Dict)
import Direction2d
import Length exposing (Length)
import Model.Board as Board
    exposing
        ( Cell
        , OrthogonalDirection(..)
        , tileSize
        )
import Model.Car exposing (CarKind(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMBoundingBox2d, LMPoint2d)
import Point2d
import Quantity
import Vector2d


type alias Lot =
    { content : Building
    , width : Length
    , height : Length
    , position : LMPoint2d
    , entryDetails : EntryDetails
    , anchor : Anchor
    }


type alias NewLot =
    { content : Building
    , width : Length
    , height : Length
    }


type alias Lots =
    Dict Id Lot


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    ( Cell, OrthogonalDirection )


type alias Building =
    { kind : BuildingKind
    , entryDirection : OrthogonalDirection
    }


type alias EntryDetails =
    { width : Length
    , height : Length
    , entryPoint : LMPoint2d
    , parkingSpot : LMPoint2d
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


halfTile : Length
halfTile =
    Quantity.half tileSize


drivewaySize : Length
drivewaySize =
    tileSize |> Quantity.divideBy 6


drivewayOverlap : Length
drivewayOverlap =
    tileSize |> Quantity.divideBy 16


allLots : List NewLot
allLots =
    [ { content = Building ResidentialA Down, width = tileSize, height = tileSize }
    , { content = Building ResidentialB Right, width = tileSize, height = tileSize }
    , { content = Building ResidentialC Left, width = tileSize, height = tileSize }
    , { content = Building ResidentialD Up, width = tileSize, height = tileSize }
    , { content = Building ResidentialE Down
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize |> Quantity.multiplyBy 2
      }
    , { content = Building TwoByOneTest Left
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize
      }
    , { content = Building ThreeByThreeTest Right
      , width = tileSize |> Quantity.multiplyBy 3
      , height = tileSize |> Quantity.multiplyBy 3
      }
    , { content = Building TwoByThreeTest Up
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize |> Quantity.multiplyBy 3
      }
    ]


fromNewLot : ( NewLot, Cell ) -> Lot
fromNewLot ( newLot, aCell ) =
    let
        anchor =
            ( aCell, Board.oppositeOrthogonalDirection newLot.content.entryDirection )

        position =
            bottomLeftCorner anchor newLot
    in
    { content = newLot.content
    , width = newLot.width
    , height = newLot.height
    , position = position
    , entryDetails = entryDetails anchor newLot
    , anchor = anchor
    }


bottomLeftCorner : Anchor -> NewLot -> LMPoint2d
bottomLeftCorner ( aCell, aDir ) { width, height } =
    let
        origin =
            aCell
                |> Board.nextOrthogonalCell aDir
                |> Board.cellBottomLeftCorner

        displacement =
            Vector2d.xy
                (tileSize |> Quantity.minus width)
                (tileSize |> Quantity.minus height)

        adjustedForVerticalEntry =
            origin |> Point2d.translateBy displacement
    in
    case aDir of
        Down ->
            adjustedForVerticalEntry

        Left ->
            adjustedForVerticalEntry

        _ ->
            origin


entryDetails : Anchor -> NewLot -> EntryDetails
entryDetails anchor newLot =
    let
        ( width, height ) =
            if Board.isVerticalDirection <| Tuple.second anchor then
                ( halfTile, drivewaySize )

            else
                ( drivewaySize, halfTile )

        entryPoint =
            case newLot.content.entryDirection of
                Up ->
                    Point2d.xy
                        (newLot.width |> Quantity.minus halfTile)
                        (newLot.height |> Quantity.plus drivewayOverlap)

                Right ->
                    Point2d.xy
                        (newLot.width |> Quantity.plus drivewayOverlap)
                        (newLot.height |> Quantity.minus halfTile)

                Down ->
                    Point2d.xy
                        halfTile
                        (Quantity.negate drivewayOverlap)

                Left ->
                    Point2d.xy
                        (Quantity.negate drivewayOverlap)
                        halfTile
    in
    { width = width
    , height = height
    , entryPoint = entryPoint
    , parkingSpot = parkingSpot anchor newLot
    }


parkingSpot : Anchor -> NewLot -> LMPoint2d
parkingSpot anchor newLot =
    let
        origin =
            Board.cellBottomLeftCorner (entryCell anchor)

        ( shiftX, shiftY ) =
            case newLot.content.entryDirection of
                Up ->
                    ( halfTile, tileSize )

                Right ->
                    ( tileSize, halfTile )

                Down ->
                    ( halfTile, Quantity.zero )

                Left ->
                    ( Quantity.zero, halfTile )

        displacement =
            Vector2d.xy shiftX shiftY
    in
    origin |> Point2d.translateBy displacement


parkingSpotOrientation : Lot -> Angle
parkingSpotOrientation lot =
    lot.content.entryDirection
        |> Board.orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


entryCell : Anchor -> Cell
entryCell anchor =
    let
        ( aCell, aDir ) =
            anchor
    in
    Board.nextOrthogonalCell aDir aCell


anchorCell : Lot -> Cell
anchorCell lot =
    Tuple.first lot.anchor


boundingBox : Lot -> LMBoundingBox2d
boundingBox lot =
    lot.position
        |> Common.boundingBoxWithDimensions lot.width lot.height


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn (boundingBox lot) (Board.cellBoundingBox cell)


resident : Lot -> Maybe CarKind
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just <|
                Sedan
                    { body = Color.rgb255 47 149 208
                    , detail = Color.rgb255 41 141 198
                    , shade = Color.rgb255 208 147 173
                    , edge = Color.rgb255 22 98 142
                    }

        ResidentialB ->
            Just <|
                Sedan
                    { body = Color.rgb255 232 106 23
                    , detail = Color.rgb255 191 100 40
                    , shade = Color.rgb255 217 163 125
                    , edge = Color.rgb255 159 73 16
                    }

        ResidentialC ->
            Just <|
                Sedan
                    { body = Color.rgb255 255 204 0
                    , detail = Color.rgb255 229 186 16
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 159 128 10
                    }

        ResidentialD ->
            Just <|
                Sedan
                    { body = Color.rgb255 57 194 114
                    , detail = Color.rgb255 48 182 104
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 20 119 61
                    }

        ResidentialE ->
            Just <|
                Sedan
                    { body = Color.rgb255 93 91 91
                    , detail = Color.rgb255 79 76 76
                    , shade = Color.rgb255 208 184 147
                    , edge = Color.rgb255 58 53 53
                    }

        _ ->
            Nothing
