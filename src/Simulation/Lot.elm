module Simulation.Lot exposing
    ( Lot
    , ParkingReservation
    , ParkingSpot
    , acquireParkingLock
    , build
    , constructionSite
    , findFreeParkingSpot
    , inBounds
    , parkingPermitted
    , parkingSpotById
    , parkingSpotEligibleForAll
    , parkingSpotEligibleForResident
    , parkingSpotOrientation
    , prepareParking
    , releaseParkingLock
    , reserveParkingSpot
    , unreserveParkingSpot
    )

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates, LocalCoordinates)
import CubicSpline2d exposing (CubicSpline2d)
import Data.Lots exposing (LotKind, NewLot, ParkingRestriction(..))
import Direction2d
import Frame2d exposing (Frame2d)
import Length exposing (Length)
import Lib.Collection exposing (Id)
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection(..))
import Point2d exposing (Point2d)
import Quantity
import Simulation.Splines as Splines
import Tilemap.Cell as Cell exposing (Cell)
import Vector2d


type alias Lot =
    { id : Id
    , kind : LotKind
    , width : Length
    , height : Length
    , position : Point2d Length.Meters GlobalCoordinates
    , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
    , entryPosition : Point2d Length.Meters GlobalCoordinates
    , exitPosition : Point2d Length.Meters GlobalCoordinates
    , drivewayExitDirection : OrthogonalDirection
    , parkingSpotExitDirection : OrthogonalDirection
    , parkingSpots : List ParkingSpot
    , parkingLock : Maybe Id
    }


build : NewLot -> Cell -> Id -> Lot
build newLot anchor lotId =
    let
        constructionSiteBB =
            constructionSite anchor newLot

        lotFrame =
            Common.boundingBoxToFrame constructionSiteBB
    in
    { id = lotId
    , kind = newLot.kind
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint constructionSiteBB
    , boundingBox = constructionSiteBB
    , entryPosition = Point2d.placeIn lotFrame newLot.entryPosition
    , exitPosition = Point2d.placeIn lotFrame newLot.exitPosition
    , drivewayExitDirection = newLot.drivewayExitDirection
    , parkingSpotExitDirection = newLot.parkingSpotExitDirection
    , parkingLock = Nothing
    , parkingSpots =
        newLot.parkingSpots
            |> List.indexedMap (\spotId spot -> createParkingSpot spotId newLot lotFrame spot)
            -- Prioritize resident parking, so that those spots are tried first when looking for a free spot
            |> List.sortWith
                (\spotA spotB ->
                    if spotA.parkingRestriction == ResidentParkingOnly && spotB.parkingRestriction == NoRestriction then
                        LT

                    else
                        GT
                )
    }


constructionSite : Cell -> NewLot -> BoundingBox2d Length.Meters GlobalCoordinates
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
                        Cell.size

                Left ->
                    -- Bottom left entry/exit cell
                    Vector2d.xy
                        Cell.size
                        Quantity.zero

        bottomLeftCorner =
            Point2d.translateBy displacement origin
    in
    Common.boundingBoxWithDimensions width height bottomLeftCorner


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Cell.boundingBox cell)



--
-- Parking
--


prepareParking : (ParkingSpot -> Bool) -> Id -> Lot -> Maybe ( Lot, ParkingSpot )
prepareParking permissionPredicate carId lot =
    acquireParkingLock carId lot
        |> Common.andCarry (findFreeParkingSpot permissionPredicate)



-- Lock


acquireParkingLock : Id -> Lot -> Maybe Lot
acquireParkingLock carId lot =
    case lot.parkingLock of
        Just _ ->
            Nothing

        Nothing ->
            Just { lot | parkingLock = Just carId }


releaseParkingLock : Id -> Lot -> Lot
releaseParkingLock carId lot =
    let
        nextParkingLock =
            lot.parkingLock
                |> Maybe.andThen
                    (\currentLockOwner ->
                        if currentLockOwner == carId then
                            Nothing

                        else
                            -- Retain the lock if the lock is not owned by the car in question
                            lot.parkingLock
                    )
    in
    { lot | parkingLock = nextParkingLock }



-- Parking spots


type alias ParkingSpot =
    { id : Int
    , position : Point2d Length.Meters GlobalCoordinates
    , pathFromLotEntry : List (CubicSpline2d Length.Meters GlobalCoordinates)
    , pathToLotExit : List (CubicSpline2d Length.Meters GlobalCoordinates)
    , parkingRestriction : ParkingRestriction
    , reservedBy : Maybe Id
    }


type alias ParkingReservation =
    { lotId : Id
    , parkingSpotId : Int
    }


createParkingSpot :
    Int
    -> NewLot
    -> Frame2d Length.Meters GlobalCoordinates { defines : LocalCoordinates }
    -> ( Point2d Length.Meters LocalCoordinates, ParkingRestriction )
    -> ParkingSpot
createParkingSpot id newLot lotFrame ( position, parkingRestriction ) =
    let
        splineProps =
            { parkingSpotPosition = position
            , lotEntryPosition = newLot.entryPosition
            , lotExitPosition = newLot.exitPosition
            , parkingSpotExitDirection = OrthogonalDirection.toDirection2d newLot.parkingSpotExitDirection
            , drivewayExitDirection = OrthogonalDirection.toDirection2d newLot.drivewayExitDirection
            , parkingLaneStartPosition = newLot.parkingLaneStartPosition
            , parkingLaneStartDirection = OrthogonalDirection.toDirection2d newLot.parkingLaneStartDirection
            }

        entrySpline =
            Splines.lotEntrySpline splineProps

        exitSpline =
            Splines.lotExitSpline splineProps
    in
    { id = id
    , position = Point2d.placeIn lotFrame position
    , pathFromLotEntry = entrySpline |> List.map (Splines.asGlobalSpline lotFrame)
    , pathToLotExit = exitSpline |> List.map (Splines.asGlobalSpline lotFrame)
    , parkingRestriction = parkingRestriction
    , reservedBy = Nothing
    }


parkingSpotOrientation : Lot -> Angle
parkingSpotOrientation lot =
    lot.parkingSpotExitDirection
        |> OrthogonalDirection.toDirection2d
        |> Direction2d.toAngle


parkingPermitted : (ParkingSpot -> Bool) -> Lot -> Bool
parkingPermitted permissionPredicate lot =
    case lot.parkingSpots of
        [] ->
            False

        parkingSpots ->
            List.any
                permissionPredicate
                parkingSpots


parkingSpotEligibleForResident : ParkingSpot -> Bool
parkingSpotEligibleForResident _ =
    True


parkingSpotEligibleForAll : ParkingSpot -> Bool
parkingSpotEligibleForAll parkingSpot =
    parkingSpot.parkingRestriction == NoRestriction


findFreeParkingSpot : (ParkingSpot -> Bool) -> Lot -> Maybe ParkingSpot
findFreeParkingSpot permssionPredicate lot =
    findFreeParkingSpotHelper permssionPredicate lot.parkingSpots


findFreeParkingSpotHelper : (ParkingSpot -> Bool) -> List ParkingSpot -> Maybe ParkingSpot
findFreeParkingSpotHelper permissionPredicate spots =
    case spots of
        [] ->
            Nothing

        parkingSpot :: others ->
            if parkingSpot.reservedBy == Nothing && permissionPredicate parkingSpot then
                Just parkingSpot

            else
                findFreeParkingSpotHelper permissionPredicate others


reserveParkingSpot : Id -> Int -> Lot -> Lot
reserveParkingSpot carId parkingSpotId lot =
    setParkingSpotReservation (Just carId) parkingSpotId lot


unreserveParkingSpot : Int -> Lot -> Lot
unreserveParkingSpot parkingSpotId lot =
    setParkingSpotReservation Nothing parkingSpotId lot


setParkingSpotReservation : Maybe Id -> Int -> Lot -> Lot
setParkingSpotReservation reservation parkingSpotId lot =
    case parkingSpotById lot parkingSpotId of
        Just parkingSpot ->
            let
                nextParkingSpot =
                    { parkingSpot | reservedBy = reservation }
            in
            updateParkingSpot nextParkingSpot lot

        Nothing ->
            lot


parkingSpotById : Lot -> Int -> Maybe ParkingSpot
parkingSpotById lot parkingSpotId =
    lot.parkingSpots
        |> List.filter (\parkingSpot -> parkingSpot.id == parkingSpotId)
        |> List.head


updateParkingSpot : ParkingSpot -> Lot -> Lot
updateParkingSpot parkingSpot lot =
    let
        nextParkingSpots =
            List.map (replaceParkingSpotIfIdMatches parkingSpot) lot.parkingSpots
    in
    { lot | parkingSpots = nextParkingSpots }


replaceParkingSpotIfIdMatches : ParkingSpot -> ParkingSpot -> ParkingSpot
replaceParkingSpotIfIdMatches parkingSpot comparison =
    if comparison.id == parkingSpot.id then
        parkingSpot

    else
        comparison
