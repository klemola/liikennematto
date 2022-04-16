module Model.Lot exposing
    ( Lot
    , ParkingSpot
    , build
    , constructionSite
    , inBounds
    , parkingSpotOrientation
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Data.Lots exposing (LotKind, NewLot)
import Direction2d
import Length exposing (Length)
import Model.Cell as Cell exposing (Cell)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMCubicSpline2d
        , LMPoint2d
        , LMPoint2dLocal
        , OrthogonalDirection(..)
        , orthogonalDirectionToLmDirection
        )
import Point2d
import Quantity
import Splines
import Vector2d


type alias Lot =
    { id : Id
    , kind : LotKind
    , width : Length
    , height : Length
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
    , drivewayExitDirection : OrthogonalDirection
    , parkingSpotExitDirection : OrthogonalDirection
    , parkingSpots : List ParkingSpot
    }


type alias ParkingSpot =
    { position : LMPoint2d
    , pathFromLotEntry : LMCubicSpline2d
    , pathToLotExit : LMCubicSpline2d
    , owner : Maybe Id
    , reservedBy : Maybe Id
    }


build : Id -> NewLot -> Cell -> Lot
build id newLot anchor =
    let
        constructionSiteBB =
            constructionSite anchor newLot
    in
    { id = id
    , kind = newLot.kind
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint constructionSiteBB
    , boundingBox = constructionSiteBB
    , drivewayExitDirection = newLot.drivewayExitDirection
    , parkingSpotExitDirection = newLot.parkingSpotExitDirection
    , parkingSpots =
        List.map
            (createParkingSpot newLot constructionSiteBB)
            newLot.parkingSpots
    }


createParkingSpot : NewLot -> LMBoundingBox2d -> LMPoint2dLocal -> ParkingSpot
createParkingSpot newLot constructionSiteBB spot =
    let
        lotFrame =
            Common.boundingBoxToFrame constructionSiteBB

        entrySpline =
            Splines.lotEntrySpline
                { parkingSpotPosition = spot
                , lotEntryPosition = newLot.entryPosition
                , lotExitPosition = newLot.exitPosition
                , parkingSpotExitDirection = newLot.parkingSpotExitDirection
                , drivewayExitDirection = newLot.drivewayExitDirection
                }

        exitSpline =
            Splines.lotExitSpline
                { parkingSpotPosition = spot
                , lotEntryPosition = newLot.entryPosition
                , lotExitPosition = newLot.exitPosition
                , parkingSpotExitDirection = newLot.parkingSpotExitDirection
                , drivewayExitDirection = newLot.drivewayExitDirection
                }
    in
    { position = Point2d.placeIn lotFrame spot
    , pathFromLotEntry = Splines.asGlobalSpline entrySpline lotFrame
    , pathToLotExit = Splines.asGlobalSpline exitSpline lotFrame
    , owner = Nothing
    , reservedBy = Nothing
    }


constructionSite : Cell -> NewLot -> LMBoundingBox2d
constructionSite anchor { width, height, drivewayExitDirection } =
    let
        origin =
            Cell.bottomLeftCorner anchor

        displacement =
            case drivewayExitDirection of
                Up ->
                    -- Top right entry/exit cell
                    Vector2d.xy
                        (width
                            |> Quantity.minus Cell.size
                            |> Quantity.negate
                        )
                        (Quantity.negate height)

                Right ->
                    -- Bottom right entry/exit cell
                    Vector2d.xy
                        (Quantity.negate width)
                        Quantity.zero

                Down ->
                    -- Bottom left entry/exit cell
                    Vector2d.xy
                        Quantity.zero
                        (Quantity.negate Cell.size)

                Left ->
                    -- Bottom left entry/exit cell
                    Vector2d.xy
                        Cell.size
                        Quantity.zero

        bottomLeftCorner =
            Point2d.translateBy displacement origin
    in
    Common.boundingBoxWithDimensions width height bottomLeftCorner


parkingSpotOrientation : Lot -> Angle
parkingSpotOrientation lot =
    lot.parkingSpotExitDirection
        |> orthogonalDirectionToLmDirection
        |> Direction2d.toAngle


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Cell.boundingBox cell)
