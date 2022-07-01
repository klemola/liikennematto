module Splines exposing
    ( CurveKind(..)
    , LotSplineProperties
    , PathParameters
    , asGlobalSpline
    , curveSpline
    , lotEntrySpline
    , lotExitSpline
    , straightSpline
    , toNode
    )

import Angle
import Common
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Length exposing (Length)
import Model.Geometry
    exposing
        ( LMCubicSpline2d
        , LMCubicSpline2dLocal
        , LMDirection2d
        , LMDirection2dLocal
        , LMFrame2d
        , LMPoint2d
        , LMPoint2dLocal
        )
import Model.RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Point2d exposing (Point2d)
import Quantity
import Vector2d


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


asGlobalSpline : LMFrame2d -> LMCubicSpline2dLocal -> LMCubicSpline2d
asGlobalSpline frame localSpline =
    CubicSpline2d.placeIn frame localSpline


type alias PathParameters =
    { origin : LMPoint2d
    , direction : LMDirection2d
    }


toNode : PathParameters -> RNNodeContext -> LMCubicSpline2d
toNode { direction, origin } { node } =
    let
        target =
            node.label.position

        angleDegreesToTarget =
            origin
                |> Common.angleFromDirection direction target
                |> Quantity.abs
    in
    if node.label.kind == DeadendExit then
        uTurnSpline origin target direction

    else if angleDegreesToTarget |> Quantity.lessThan (Angle.radians 0.1) then
        straightSpline origin target

    else
        curveSpline Natural origin target direction


uTurnSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> CubicSpline2d Length.Meters a
uTurnSpline origin target direction =
    let
        startHandleCp =
            Point2d.translateIn direction uTurnDistance origin

        endHandleCp =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin startHandleCp endHandleCp target


straightSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> CubicSpline2d Length.Meters a
straightSpline origin target =
    CubicSpline2d.fromEndpoints origin Vector2d.zero target Vector2d.zero


sameDirectionSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> CubicSpline2d Length.Meters a
sameDirectionSpline origin target direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        magnitude =
            Quantity.half distanceToTarget

        startHandleCp =
            origin |> Point2d.translateIn direction magnitude

        endHandleCp =
            target |> Point2d.translateIn (Direction2d.reverse direction) magnitude
    in
    CubicSpline2d.fromControlPoints origin startHandleCp endHandleCp target


type CurveKind
    = Natural
    | Geometric


curveSpline : CurveKind -> Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> CubicSpline2d Length.Meters a
curveSpline kind origin target direction =
    let
        rightAnglePos =
            Common.rightAnglePosition origin target direction

        ( startHandleCp, endHandleCp ) =
            case kind of
                Natural ->
                    ( Point2d.midpoint origin rightAnglePos
                    , Point2d.midpoint rightAnglePos target
                    )

                Geometric ->
                    ( rightAnglePos, rightAnglePos )
    in
    CubicSpline2d.fromControlPoints origin startHandleCp endHandleCp target


type alias LotSplineProperties =
    { parkingSpotPosition : LMPoint2dLocal
    , lotEntryPosition : LMPoint2dLocal
    , lotExitPosition : LMPoint2dLocal
    , parkingLaneStartPosition : LMPoint2dLocal
    , parkingSpotExitDirection : LMDirection2dLocal
    , drivewayExitDirection : LMDirection2dLocal
    }


lotEntrySpline : LotSplineProperties -> List LMCubicSpline2dLocal
lotEntrySpline { parkingSpotPosition, lotEntryPosition, parkingSpotExitDirection, parkingLaneStartPosition, drivewayExitDirection } =
    let
        startDirection =
            drivewayExitDirection |> Direction2d.reverse
    in
    if parkingSkipsParkingLane parkingLaneStartPosition parkingSpotPosition drivewayExitDirection then
        [ curveSpline Geometric lotEntryPosition parkingSpotPosition startDirection ]

    else if parkingSpotExitDirection == drivewayExitDirection then
        [ sameDirectionSpline lotEntryPosition parkingSpotPosition startDirection ]

    else
        [ sameDirectionSpline lotEntryPosition parkingLaneStartPosition startDirection
        , curveSpline Geometric parkingLaneStartPosition parkingSpotPosition startDirection
        ]


lotExitSpline : LotSplineProperties -> List LMCubicSpline2dLocal
lotExitSpline { parkingSpotPosition, lotExitPosition, parkingSpotExitDirection, parkingLaneStartPosition, drivewayExitDirection } =
    let
        startDirection =
            parkingSpotExitDirection
    in
    if parkingSkipsParkingLane parkingLaneStartPosition parkingSpotPosition drivewayExitDirection then
        [ curveSpline Geometric parkingSpotPosition lotExitPosition startDirection ]

    else if parkingSpotExitDirection == drivewayExitDirection then
        [ sameDirectionSpline parkingSpotPosition lotExitPosition startDirection ]

    else
        [ curveSpline Geometric parkingSpotPosition parkingLaneStartPosition startDirection
        , sameDirectionSpline parkingLaneStartPosition lotExitPosition drivewayExitDirection
        ]


parkingSkipsParkingLane : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> Bool
parkingSkipsParkingLane parkingLaneStartPosition parkingSpotPosition lotExitDirection =
    parkingSpotPosition |> Common.isInTheNormalPlaneOf lotExitDirection parkingLaneStartPosition
