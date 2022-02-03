module Model.Lot exposing
    ( Anchor
    , Building
    , BuildingKind(..)
    , Lot
    , Lots
    , NewLot
    , allLots
    , build
    , createAnchor
    , inBounds
    , newLotBuildArea
    , parkingSpotOrientation
    , resident
    )

import Angle exposing (Angle)
import BoundingBox2d
import Common
import Data.CarAssets exposing (sedan)
import Dict exposing (Dict)
import Direction2d
import Length exposing (Length)
import Model.Car exposing (CarMake)
import Model.Entity exposing (Id)
import Model.Geometry
    exposing
        ( LMBoundingBox2d
        , LMPoint2d
        , OrthogonalDirection(..)
        , isVerticalDirection
        , oppositeOrthogonalDirection
        , orthogonalDirectionToLmDirection
        )
import Model.Tile exposing (tileSize)
import Model.Tilemap as Tilemap exposing (Cell)
import Point2d
import Quantity
import Vector2d


type alias Lot =
    { content : Building
    , width : Length
    , height : Length
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
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
    { anchorCell : Cell
    , entryCell : Cell
    , anchorDirection : OrthogonalDirection
    }


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
    Quantity.half tileSize


drivewaySize : Length
drivewaySize =
    tileSize |> Quantity.divideBy 6


drivewayOverlap : Length
drivewayOverlap =
    tileSize |> Quantity.divideBy 16


allLots : List NewLot
allLots =
    [ { content = Building ResidentialA Down, width = tileSize, height = tileSize }
    , { content = Building ResidentialB Right, width = tileSize, height = tileSize }
    , { content = Building ResidentialC Left, width = tileSize, height = tileSize }
    , { content = Building ResidentialD Up, width = tileSize, height = tileSize }
    , { content = Building ResidentialE Down
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize |> Quantity.multiplyBy 2
      }
    , { content = Building TwoByOneTest Left
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize
      }
    , { content = Building ThreeByThreeTest Right
      , width = tileSize |> Quantity.multiplyBy 3
      , height = tileSize |> Quantity.multiplyBy 3
      }
    , { content = Building TwoByThreeTest Up
      , width = tileSize |> Quantity.multiplyBy 2
      , height = tileSize |> Quantity.multiplyBy 3
      }
    ]


build : NewLot -> Anchor -> Lot
build newLot anchor =
    let
        buildAreaBB =
            newLotBuildArea anchor newLot
    in
    { content = newLot.content
    , width = newLot.width
    , height = newLot.height
    , position = BoundingBox2d.centerPoint buildAreaBB
    , boundingBox = buildAreaBB
    , entryDetails = entryDetails anchor newLot
    , anchor = anchor
    }


createAnchor : NewLot -> Cell -> Maybe Anchor
createAnchor newLot anchorCell =
    let
        anchorDirection =
            oppositeOrthogonalDirection newLot.content.entryDirection
    in
    Tilemap.nextOrthogonalCell anchorDirection anchorCell
        |> Maybe.map
            (\entryCell ->
                { anchorCell = anchorCell
                , anchorDirection = anchorDirection
                , entryCell = entryCell
                }
            )


newLotBuildArea : Anchor -> NewLot -> LMBoundingBox2d
newLotBuildArea { anchorDirection, entryCell } { width, height } =
    let
        origin =
            Tilemap.cellBottomLeftCorner entryCell

        displacement =
            Vector2d.xy
                (tileSize |> Quantity.minus width)
                (tileSize |> Quantity.minus height)

        adjustedForVerticalEntry =
            origin |> Point2d.translateBy displacement

        bottomLeftCorner =
            case anchorDirection of
                Down ->
                    adjustedForVerticalEntry

                Left ->
                    adjustedForVerticalEntry

                _ ->
                    origin
    in
    Common.boundingBoxWithDimensions width height bottomLeftCorner


entryDetails : Anchor -> NewLot -> EntryDetails
entryDetails anchor newLot =
    let
        ( width, height ) =
            if isVerticalDirection anchor.anchorDirection then
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
parkingSpot { entryCell } newLot =
    let
        origin =
            Tilemap.cellBottomLeftCorner entryCell

        ( shiftX, shiftY ) =
            case newLot.content.entryDirection of
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
    lot.content.entryDirection
        |> orthogonalDirectionToLmDirection
        |> Direction2d.rotateClockwise
        |> Direction2d.toAngle


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    BoundingBox2d.isContainedIn lot.boundingBox (Tilemap.cellBoundingBox cell)


resident : Lot -> Maybe CarMake
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just <|
                sedan

        ResidentialB ->
            Just <|
                sedan

        ResidentialC ->
            Just <|
                sedan

        ResidentialD ->
            Just <|
                sedan

        ResidentialE ->
            Just <|
                sedan

        _ ->
            Nothing
