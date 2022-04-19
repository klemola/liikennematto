module Splines exposing
    ( asGlobalSpline
    , lotEntrySpline
    , lotExitSpline
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
        , LMFrame2d
        , LMPoint2d
        , LMPoint2dLocal
        , OrthogonalDirection(..)
        , orthogonalDirectionToLmDirection
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
    , parkingSpotExitDirection : OrthogonalDirection
    , drivewayExitDirection : OrthogonalDirection
    }


lotEntrySpline : LotSplineProperties -> List LMCubicSpline2dLocal
lotEntrySpline { parkingSpotPosition, lotEntryPosition, parkingSpotExitDirection } =
    let
        startHandleCp =
            Point2d.origin

        endHandleCp =
            Point2d.origin
    in
    [ CubicSpline2d.fromControlPoints lotEntryPosition startHandleCp endHandleCp parkingSpotPosition ]


lotExitSpline : LotSplineProperties -> List LMCubicSpline2dLocal
lotExitSpline { parkingSpotPosition, lotExitPosition, parkingSpotExitDirection, parkingLaneStartPosition, drivewayExitDirection } =
    let
        startDirection =
            orthogonalDirectionToLmDirection parkingSpotExitDirection
    in
    if Point2d.distanceFrom parkingSpotPosition lotExitPosition |> Quantity.lessThanOrEqualTo (Length.meters 6) then
        [ curveSpline Geometric parkingSpotPosition lotExitPosition startDirection ]

    else if parkingSpotExitDirection == drivewayExitDirection then
        [ sameDirectionSpline parkingSpotPosition lotExitPosition startDirection ]

    else
        [ curveSpline Geometric parkingSpotPosition parkingLaneStartPosition startDirection
        , sameDirectionSpline parkingLaneStartPosition lotExitPosition (orthogonalDirectionToLmDirection drivewayExitDirection)
        ]
