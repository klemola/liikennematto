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
import BoundingBox2d
import Collection exposing (Id)
import Common
import Data.Lots exposing (LotKind, NewLot, ParkingRestriction(..))
import Direction2d
import Length exposing (Length)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMCubicSpline2d
        , LMFrame2d
        , LMPoint2d
        , LMPoint2dLocal
        , OrthogonalDirection(..)
        , orthogonalDirectionToLmDirection
        )
import Point2d
import Quantity
import Splines
import Tilemap.Cell as Cell exposing (Cell)
import Vector2d


type alias Lot =
    { id : Id
    , kind : LotKind
    , width : Length
    , height : Length
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
    , entryPosition : LMPoint2d
    , exitPosition : LMPoint2d
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
    , position : LMPoint2d
    , pathFromLotEntry : List LMCubicSpline2d
    , pathToLotExit : List LMCubicSpline2d
    , parkingRestriction : ParkingRestriction
    , reservedBy : Maybe Id
    }


type alias ParkingReservation =
    { lotId : Id
    , parkingSpotId : Int
    }


createParkingSpot : Int -> NewLot -> LMFrame2d -> ( LMPoint2dLocal, ParkingRestriction ) -> ParkingSpot
createParkingSpot id newLot lotFrame ( position, parkingRestriction ) =
    let
        splineProps =
            { parkingSpotPosition = position
            , lotEntryPosition = newLot.entryPosition
            , lotExitPosition = newLot.exitPosition
            , parkingSpotExitDirection = orthogonalDirectionToLmDirection newLot.parkingSpotExitDirection
            , drivewayExitDirection = orthogonalDirectionToLmDirection newLot.drivewayExitDirection
            , parkingLaneStartPosition = newLot.parkingLaneStartPosition
            , parkingLaneStartDirection = orthogonalDirectionToLmDirection newLot.parkingLaneStartDirection
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
        |> orthogonalDirectionToLmDirection
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
