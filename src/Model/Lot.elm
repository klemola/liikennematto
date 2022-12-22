module Model.Lot exposing
    ( Lot
    , ParkingReservation
    , ParkingSpot
    , acquireParkingLock
    , attemptParking
    , build
    , claimParkingSpot
    , constructionSite
    , findFreeParkingSpot
    , hasParkingLockSet
    , inBounds
    , parkingPermitted
    , parkingSpotById
    , parkingSpotOrientation
    , releaseParkingLock
    , reserveParkingSpot
    , unreserveParkingSpot
    , updateParkingSpot
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Data.Colors exposing (ThemeColor)
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
            (\spotId position -> createParkingSpot spotId newLot constructionSiteBB position)
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


attemptParking : Id -> Lot -> Maybe ParkingSpot
attemptParking carId lot =
    if hasParkingLockSet lot then
        Nothing

    else
        findFreeParkingSpot carId lot



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
    , owner : Maybe Id
    , reservedBy : Maybe Id
    }


type alias ParkingReservation =
    { lotId : Id
    , parkingSpotId : Id
    }


createParkingSpot : Int -> NewLot -> LMBoundingBox2d -> LMPoint2dLocal -> ParkingSpot
createParkingSpot idx newLot constructionSiteBB spot =
    let
        lotFrame =
            Common.boundingBoxToFrame constructionSiteBB

        splineProps =
            { parkingSpotPosition = spot
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
    , position = Point2d.placeIn lotFrame spot
    , pathFromLotEntry = entrySpline |> List.map (Splines.asGlobalSpline lotFrame)
    , pathToLotExit = exitSpline |> List.map (Splines.asGlobalSpline lotFrame)
    , owner = Nothing
    , reservedBy = Nothing
    }


parkingSpotOrientation : Lot -> Angle
parkingSpotOrientation lot =
    lot.parkingSpotExitDirection
        |> orthogonalDirectionToLmDirection
        |> Direction2d.toAngle


parkingPermitted : Id -> Lot -> Bool
parkingPermitted carId lot =
    case lot.parkingSpots of
        [] ->
            False

        parkingSpots ->
            parkingSpots |> List.any (\spot -> spot.owner == Nothing || spot.owner == Just carId)


findFreeParkingSpot : Id -> Lot -> Maybe ParkingSpot
findFreeParkingSpot carId lot =
    findFreeParkingSpotHelper carId lot.parkingSpots


findFreeParkingSpotHelper : Id -> List ParkingSpot -> Maybe ParkingSpot
findFreeParkingSpotHelper carId spots =
    case spots of
        [] ->
            Nothing

        parkingSpot :: others ->
            case ( parkingSpot.owner, parkingSpot.reservedBy ) of
                ( Nothing, Nothing ) ->
                    Just parkingSpot

                ( Just ownerId, Nothing ) ->
                    if carId == ownerId then
                        Just parkingSpot

                    else
                        findFreeParkingSpotHelper carId others

                -- the parking spot is reserved or owned by someone else
                _ ->
                    findFreeParkingSpotHelper carId others


claimParkingSpot : Id -> Lot -> Maybe ParkingSpot
claimParkingSpot carId lot =
    -- Claims the first parking spot (none will be reserved when the lot has just been created)
    findFreeParkingSpot carId lot |> Maybe.map (\parkingSpot -> { parkingSpot | owner = Just carId })


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
