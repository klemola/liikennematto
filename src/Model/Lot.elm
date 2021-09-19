module Model.Lot exposing
    ( Anchor
    , Building
    , BuildingKind(..)
    , Lot
    , Lots
    , NewLot
    , anchorCell
    , bottomLeftCorner
    , boundingBox
    , entryDetails
    , fromNewLot
    , inBounds
    , parkingSpotOrientation
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Config exposing (tileSizeInMeters)
import Dict exposing (Dict)
import Direction2d
import Length exposing (Length)
import Model.Cell as Cell exposing (Cell, OrthogonalDirection(..))
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
    Quantity.half tileSizeInMeters


drivewaySize : Length
drivewaySize =
    tileSizeInMeters |> Quantity.divideBy 6


drivewayOverlap : Length
drivewayOverlap =
    tileSizeInMeters |> Quantity.divideBy 16


fromNewLot : ( NewLot, Cell ) -> Lot
fromNewLot ( newLot, aCell ) =
    let
        anchor =
            ( aCell, Cell.oppositeOrthogonalDirection newLot.content.entryDirection )

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
                |> Cell.next aDir
                |> Cell.bottomLeftCorner

        displacement =
            Vector2d.xy
                (tileSizeInMeters |> Quantity.minus width)
                (tileSizeInMeters |> Quantity.minus height)

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
            if Cell.isVertical <| Tuple.second anchor then
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
            Cell.bottomLeftCorner (entryCell anchor)

        ( shiftX, shiftY ) =
            case newLot.content.entryDirection of
                Up ->
                    ( halfTile, tileSizeInMeters )

                Right ->
                    ( tileSizeInMeters, halfTile )

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
        |> Cell.orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


entryCell : Anchor -> Cell
entryCell anchor =
    let
        ( aCell, aDir ) =
            anchor
    in
    Cell.next aDir aCell


anchorCell : Lot -> Cell
anchorCell lot =
    Tuple.first lot.anchor


boundingBox : Lot -> LMBoundingBox2d
boundingBox lot =
    lot.position
        |> Common.boundingBoxWithDimensions lot.width lot.height


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn (boundingBox lot) (Cell.boundingBox cell)
