module Model.Lot exposing
    ( Lot
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
        , LMPoint2d
        , OrthogonalDirection(..)
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
    }


build : Id -> NewLot -> Cell -> Lot
build id newLot anchor =
    let
        buildAreaBB =
            constructionSite anchor newLot
    in
    { id = id
    , kind = newLot.kind
    , drivewayExit = newLot.drivewayExit
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint buildAreaBB
    , boundingBox = buildAreaBB

    -- TODO: temp value
    , parkingSpot = BoundingBox2d.centerPoint buildAreaBB
    }


constructionSite : Cell -> NewLot -> LMBoundingBox2d
constructionSite anchor { width, height, drivewayExit } =
    let
        origin =
            Cell.bottomLeftCorner anchor

        displacement =
            case drivewayExit of
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
    lot.drivewayExit
        |> orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Cell.boundingBox cell)
