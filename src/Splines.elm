module Splines exposing
    ( asGlobalSpline
    , lotEntrySpline
    , lotExitSpline
    , toNode
    )

import Angle
import Common
import CubicSpline2d
import Direction2d
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
import Point2d
import Quantity
import Vector2d


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


asGlobalSpline : LMCubicSpline2dLocal -> LMFrame2d -> LMCubicSpline2d
asGlobalSpline localSpline frame =
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
        curveSpline origin target direction


uTurnSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LMCubicSpline2d
uTurnSpline origin target direction =
    let
        handleCp1 =
            Point2d.translateIn direction uTurnDistance origin

        handleCp2 =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target


straightSpline : LMPoint2d -> LMPoint2d -> LMCubicSpline2d
straightSpline origin target =
    CubicSpline2d.fromEndpoints origin Vector2d.zero target Vector2d.zero


curveSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LMCubicSpline2d
curveSpline origin target direction =
    let
        rightAnglePos =
            Common.rightAnglePosition origin target direction

        handleCp1 =
            Point2d.midpoint origin rightAnglePos

        handleCp2 =
            Point2d.midpoint rightAnglePos target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target


type alias LotSplineProperties =
    { parkingSpotPosition : LMPoint2dLocal
    , lotEntryPosition : LMPoint2dLocal
    , lotExitPosition : LMPoint2dLocal
    , parkingSpotExitDirection : OrthogonalDirection
    , drivewayExitDirection : OrthogonalDirection
    }


lotEntrySpline : LotSplineProperties -> LMCubicSpline2dLocal
lotEntrySpline { parkingSpotPosition, lotEntryPosition, parkingSpotExitDirection } =
    let
        handleCp1 =
            Point2d.origin

        handleCp2 =
            Point2d.origin
    in
    CubicSpline2d.fromControlPoints lotEntryPosition handleCp1 handleCp2 parkingSpotPosition


lotExitSpline : LotSplineProperties -> LMCubicSpline2dLocal
lotExitSpline { parkingSpotPosition, lotExitPosition, parkingSpotExitDirection, drivewayExitDirection } =
    let
        startDirection =
            orthogonalDirectionToLmDirection parkingSpotExitDirection

        distanceToTarget =
            Point2d.distanceFrom parkingSpotPosition lotExitPosition

        rightAnglePos =
            Common.rightAnglePosition parkingSpotPosition lotExitPosition startDirection

        ( handleCp1, handleCp2 ) =
            if drivewayExitDirection == parkingSpotExitDirection then
                ( parkingSpotPosition |> Point2d.translateIn startDirection (Quantity.half distanceToTarget)
                , lotExitPosition |> Point2d.translateIn (Direction2d.reverse startDirection) (Quantity.half distanceToTarget)
                )

            else
                ( rightAnglePos, rightAnglePos )
    in
    CubicSpline2d.fromControlPoints
        parkingSpotPosition
        handleCp1
        handleCp2
        lotExitPosition
