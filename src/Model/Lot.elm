module Model.Lot exposing
    ( Lot
    , ParkingReservation
    , ParkingSpot
    , acquireParkingLock
    , attemptParking
    , build
    , constructionSite
    , findFreeParkingSpot
    , hasParkingLockSet
    , inBounds
    , parkingPermitted
    , parkingSpotById
    , parkingSpotEligibleForAll
    , parkingSpotEligibleForResident
    , parkingSpotOrientation
    , releaseParkingLock
    , reserveParkingSpot
    , unreserveParkingSpot
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Data.Colors exposing (ThemeColor)
import Data.Lots exposing (LotKind, NewLot, ParkingRestriction(..))
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
    , themeColor : ThemeColor
    , width : Length
    , height : Length
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
    , drivewayExitDirection : OrthogonalDirection
    , parkingSpotExitDirection : OrthogonalDirection
    , parkingSpots : List ParkingSpot
    , parkingLock : Maybe Id
    }


build : Id -> NewLot -> Cell -> Lot
build lotId newLot anchor =
    let
        constructionSiteBB =
            constructionSite anchor newLot
    in
    { id = lotId
    , kind = newLot.kind
    , themeColor = newLot.themeColor
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint constructionSiteBB
    , boundingBox = constructionSiteBB
    , drivewayExitDirection = newLot.drivewayExitDirection
    , parkingSpotExitDirection = newLot.parkingSpotExitDirection
    , parkingSpots =
        List.indexedMap
            (\spotId spot -> createParkingSpot spotId newLot constructionSiteBB spot)
            newLot.parkingSpots
    , parkingLock = Nothing
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


attemptParking : (ParkingSpot -> Bool) -> Lot -> Maybe ParkingSpot
attemptParking permissionPredicate lot =
    if hasParkingLockSet lot then
        Nothing

    else
        findFreeParkingSpot permissionPredicate lot



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


hasParkingLockSet : Lot -> Bool
hasParkingLockSet lot =
    lot.parkingLock /= Nothing



-- Parking spots


type alias ParkingSpot =
    { id : Id
    , position : LMPoint2d
    , pathFromLotEntry : List LMCubicSpline2d
    , pathToLotExit : List LMCubicSpline2d
    , parkingRestriction : ParkingRestriction
    , reservedBy : Maybe Id
    }


type alias ParkingReservation =
    { lotId : Id
    , parkingSpotId : Id
    }


createParkingSpot : Int -> NewLot -> LMBoundingBox2d -> ( LMPoint2dLocal, ParkingRestriction ) -> ParkingSpot
createParkingSpot idx newLot constructionSiteBB ( position, parkingRestriction ) =
    let
        lotFrame =
            Common.boundingBoxToFrame constructionSiteBB

        splineProps =
            { parkingSpotPosition = position
            , lotEntryPosition = newLot.entryPosition
            , lotExitPosition = newLot.exitPosition
            , parkingSpotExitDirection = orthogonalDirectionToLmDirection newLot.parkingSpotExitDirection
            , drivewayExitDirection = orthogonalDirectionToLmDirection newLot.drivewayExitDirection
            , parkingLaneStartPosition = newLot.parkingLaneStartPosition
            }

        entrySpline =
            Splines.lotEntrySpline splineProps

        exitSpline =
            Splines.lotExitSpline splineProps
    in
    { id = idx
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


reserveParkingSpot : Id -> Id -> Lot -> Lot
reserveParkingSpot carId parkingSpotId lot =
    setParkingSpotReservation (Just carId) parkingSpotId lot


unreserveParkingSpot : Id -> Lot -> Lot
unreserveParkingSpot parkingSpotId lot =
    setParkingSpotReservation Nothing parkingSpotId lot


setParkingSpotReservation : Maybe Id -> Id -> Lot -> Lot
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


parkingSpotById : Lot -> Id -> Maybe ParkingSpot
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
