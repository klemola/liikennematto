module Model.Lot exposing
    ( Lot
    , ParkingSpot
    , build
    , claimParkingSpot
    , constructionSite
    , findFreeParkingSpot
    , inBounds
    , parkingSpotOrientation
    , updateParkingSpot
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
    { id : Id
    , position : LMPoint2d
    , pathFromLotEntry : List LMCubicSpline2d
    , pathToLotExit : List LMCubicSpline2d
    , owner : Maybe Id
    , reservedBy : Maybe Id
    }


build : Id -> NewLot -> Cell -> Lot
build lotId newLot anchor =
    let
        constructionSiteBB =
            constructionSite anchor newLot
    in
    { id = lotId
    , kind = newLot.kind
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

                -- the parking spot is reserved (might also be owned by someone)
                _ ->
                    findFreeParkingSpotHelper carId others


claimParkingSpot : Id -> Lot -> Maybe ParkingSpot
claimParkingSpot carId lot =
    findFreeParkingSpot carId lot |> Maybe.map (\parkingSpot -> { parkingSpot | owner = Just carId })


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


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Cell.boundingBox cell)
