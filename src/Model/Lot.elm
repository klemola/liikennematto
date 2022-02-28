module Model.Lot exposing
    ( Lot
    , LotKind(..)
    , NewLot
    , build
    , connectToCell
    , inBounds
    , newLotBuildArea
    , parkingSpotOrientation
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Direction2d
import Length exposing (Length)
import Model.Cell as Cell exposing (Anchor, Cell)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , OrthogonalDirection(..)
        , oppositeOrthogonalDirection
        , orthogonalDirectionToLmDirection
        )
import Point2d
import Quantity
import Vector2d


type alias Lot =
    { id : Id
    , kind : LotKind
    , drivewayExit : OrthogonalDirection
    , parkingSpot : LMPoint2d
    , width : Length
    , height : Length
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
    , anchor : Anchor
    }


type alias NewLot =
    { kind : LotKind
    , drivewayExit : OrthogonalDirection
    , width : Length
    , height : Length
    }


type LotKind
    = ResidentialSingle1
    | School


halfTile : Length
halfTile =
    Quantity.half Cell.size


build : Id -> NewLot -> Anchor -> Lot
build id newLot anchor =
    let
        buildAreaBB =
            newLotBuildArea anchor newLot
    in
    { id = id
    , kind = newLot.kind
    , drivewayExit = newLot.drivewayExit
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint buildAreaBB
    , boundingBox = buildAreaBB
    , parkingSpot = parkingSpot anchor newLot
    , anchor = anchor
    }


connectToCell : NewLot -> Cell -> Maybe Anchor
connectToCell newLot anchorCell =
    let
        anchorDirection =
            oppositeOrthogonalDirection newLot.drivewayExit
    in
    Cell.nextOrthogonalCell anchorDirection anchorCell
        |> Maybe.map
            (\lotEntry ->
                { from = anchorCell
                , to = lotEntry
                , direction = anchorDirection
                }
            )


newLotBuildArea : Anchor -> NewLot -> LMBoundingBox2d
newLotBuildArea anchor { width, height } =
    let
        origin =
            Cell.bottomLeftCorner anchor.to

        displacement =
            Vector2d.xy
                (Cell.size |> Quantity.minus width)
                (Cell.size |> Quantity.minus height)

        adjustedForVerticalEntry =
            origin |> Point2d.translateBy displacement

        bottomLeftCorner =
            case anchor.direction of
                Down ->
                    adjustedForVerticalEntry

                Left ->
                    adjustedForVerticalEntry

                _ ->
                    origin
    in
    Common.boundingBoxWithDimensions width height bottomLeftCorner


parkingSpot : Anchor -> NewLot -> LMPoint2d
parkingSpot anchor newLot =
    let
        origin =
            Cell.bottomLeftCorner anchor.to

        ( shiftX, shiftY ) =
            case newLot.drivewayExit of
                Up ->
                    ( halfTile, Cell.size )

                Right ->
                    ( Cell.size, halfTile )

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
    lot.drivewayExit
        |> orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Cell.boundingBox cell)
