module Splines exposing
    ( LotSplineProperties
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
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Point2d exposing (Point2d)
import Quantity


uTurnDistance : Length
uTurnDistance =
    Length.meters 3


asGlobalSpline : LMFrame2d -> LMCubicSpline2dLocal -> LMCubicSpline2d
asGlobalSpline frame localSpline =
    CubicSpline2d.placeIn frame localSpline


type alias PathParameters =
    { origin : LMPoint2d
    , direction : LMDirection2d
    , environment : RoadNetwork.ConnectionEnvironment
    }


toNode : PathParameters -> RNNodeContext -> LMCubicSpline2d
toNode { direction, origin, environment } { node } =
    let
        target =
            node.label.position
    in
    if node.label.kind == DeadendExit then
        uTurnSpline origin target direction

    else
        let
            angleDegreesToTarget =
                origin
                    |> Common.angleFromDirection direction target
                    |> Quantity.abs
        in
        if angleDegreesToTarget |> Quantity.lessThan (Angle.radians 0.1) then
            straightSpline origin target

        else
            let
                parameter =
                    if environment == RoadNetwork.Intersection then
                        0.75

                    else
                        0.5
            in
            curveSpline origin target direction parameter


uTurnSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> CubicSpline2d Length.Meters a
uTurnSpline origin target direction =
    let
        cp1 =
            Point2d.translateIn direction uTurnDistance origin

        cp2 =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin cp1 cp2 target


straightSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> CubicSpline2d Length.Meters a
straightSpline origin target =
    let
        cp1 =
            Point2d.interpolateFrom origin target (1 / 3)

        cp2 =
            Point2d.interpolateFrom origin target (2 / 3)
    in
    CubicSpline2d.fromControlPoints origin cp1 cp2 target


mirroredSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> Float -> Direction2d a -> CubicSpline2d Length.Meters a
mirroredSpline origin target distanceMultiplier direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        yDifference =
            distanceToTarget
                |> Quantity.multiplyBy
                    (origin
                        |> Common.angleFromDirection direction target
                        |> Angle.sin
                        |> abs
                    )

        differenceOffset =
            yDifference |> Quantity.multiplyBy 0.2

        magnitude =
            distanceToTarget
                |> Quantity.multiplyBy distanceMultiplier
                |> Quantity.plus differenceOffset

        cp1 =
            origin |> Point2d.translateIn direction magnitude

        cp2 =
            target |> Point2d.translateIn (Direction2d.reverse direction) magnitude
    in
    CubicSpline2d.fromControlPoints origin cp1 cp2 target


curveSpline : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> Float -> CubicSpline2d Length.Meters a
curveSpline origin target direction parameter =
    let
        rightAnglePos =
            Common.rightAnglePosition origin target direction

        ( cp1, cp2 ) =
            ( Point2d.interpolateFrom origin rightAnglePos parameter
            , Point2d.interpolateFrom target rightAnglePos parameter
            )
    in
    CubicSpline2d.fromControlPoints origin cp1 cp2 target


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
    if parkingSpotExitDirection == drivewayExitDirection then
        [ mirroredSpline lotEntryPosition parkingSpotPosition 0.5 startDirection ]

    else if parkingSpotCloseToParkingLaneStart lotEntryPosition parkingSpotPosition then
        [ mirroredSpline lotEntryPosition parkingLaneStartPosition 0.33 startDirection
        , curveSpline parkingLaneStartPosition parkingSpotPosition startDirection 0.8
        ]

    else
        let
            parkingSpotSplineStart =
                parkingSpotSplineStartPosition
                    parkingLaneStartPosition
                    parkingSpotPosition
                    startDirection
        in
        [ mirroredSpline lotEntryPosition parkingLaneStartPosition 0.33 startDirection
        , straightSpline parkingLaneStartPosition parkingSpotSplineStart
        , curveSpline parkingSpotSplineStart parkingSpotPosition startDirection 0.7
        ]


lotExitSpline : LotSplineProperties -> List LMCubicSpline2dLocal
lotExitSpline { parkingSpotPosition, lotExitPosition, parkingSpotExitDirection, parkingLaneStartPosition, drivewayExitDirection } =
    let
        startDirection =
            parkingSpotExitDirection
    in
    if parkingSpotExitDirection == drivewayExitDirection then
        [ mirroredSpline parkingSpotPosition lotExitPosition 0.66 startDirection ]

    else if parkingSpotCloseToParkingLaneStart lotExitPosition parkingSpotPosition then
        [ curveSpline parkingSpotPosition parkingLaneStartPosition startDirection 0.8
        , mirroredSpline parkingLaneStartPosition lotExitPosition 0.33 drivewayExitDirection
        ]

    else
        let
            parkingSpotSplineStart =
                parkingSpotSplineStartPosition
                    parkingLaneStartPosition
                    parkingSpotPosition
                    (drivewayExitDirection |> Direction2d.reverse)
        in
        [ curveSpline parkingSpotPosition parkingSpotSplineStart startDirection 0.7
        , straightSpline parkingSpotSplineStart parkingLaneStartPosition
        , mirroredSpline parkingLaneStartPosition lotExitPosition 0.33 drivewayExitDirection
        ]


parkingSpotSplineStartPosition : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> Point2d Length.Meters a
parkingSpotSplineStartPosition parkingLaneStartPosition parkingSpotPosition startDirection =
    let
        xDiff =
            Point2d.xCoordinate parkingLaneStartPosition
                |> Quantity.minus (Point2d.xCoordinate parkingSpotPosition)
                |> Quantity.abs

        yDiff =
            Point2d.yCoordinate parkingLaneStartPosition
                |> Quantity.minus (Point2d.yCoordinate parkingSpotPosition)
                |> Quantity.abs

        distanceToParkingSpotSplineStart =
            xDiff |> Quantity.minus yDiff
    in
    parkingLaneStartPosition |> Point2d.translateIn startDirection distanceToParkingSpotSplineStart


parkingSpotCloseToParkingLaneStart : Point2d Length.Meters a -> Point2d Length.Meters a -> Bool
parkingSpotCloseToParkingLaneStart lotEntryOrExit parkingSpotPosition =
    Point2d.distanceFrom lotEntryOrExit parkingSpotPosition |> Quantity.lessThan (Length.meters 16)
