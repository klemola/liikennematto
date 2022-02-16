module Model.Lot exposing
    ( Anchor
    , Lot
    , LotKind(..)
    , NewLot
    , build
    , createAnchor
    , inBounds
    , newLotBuildArea
    , parkingSpotOrientation
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Direction2d
import Length exposing (Length)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , OrthogonalDirection(..)
        , oppositeOrthogonalDirection
        , orthogonalDirectionToLmDirection
        )
import Model.Tile exposing (tileSize)
import Model.Tilemap as Tilemap exposing (Cell)
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


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    { cell : Cell
    , direction : OrthogonalDirection
    , lotEntry : Cell
    }


type LotKind
    = ResidentialSingle1
    | School


halfTile : Length
halfTile =
    Quantity.half tileSize


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


createAnchor : NewLot -> Cell -> Maybe Anchor
createAnchor newLot anchorCell =
    let
        anchorDirection =
            oppositeOrthogonalDirection newLot.drivewayExit
    in
    Tilemap.nextOrthogonalCell anchorDirection anchorCell
        |> Maybe.map
            (\lotEntry ->
                { cell = anchorCell
                , direction = anchorDirection
                , lotEntry = lotEntry
                }
            )


newLotBuildArea : Anchor -> NewLot -> LMBoundingBox2d
newLotBuildArea anchor { width, height } =
    let
        origin =
            Tilemap.cellBottomLeftCorner anchor.lotEntry

        displacement =
            Vector2d.xy
                (tileSize |> Quantity.minus width)
                (tileSize |> Quantity.minus height)

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
            Tilemap.cellBottomLeftCorner anchor.lotEntry

        ( shiftX, shiftY ) =
            case newLot.drivewayExit of
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
    lot.drivewayExit
        |> orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Tilemap.cellBoundingBox cell)
