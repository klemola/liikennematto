module Simulation.Splines exposing
    ( LotSplineProperties
    , PathParameters
    , asGlobalSpline
    , curveSpline
    , lotEntrySpline
    , lotExitSpline
    , parkingSpotSplineStartPoint
    , straightSpline
    , toNode
    )

import Angle
import Common exposing (GlobalCoordinates, LocalCoordinates)
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Length exposing (Length)
import Point2d exposing (Point2d)
import Quantity
import Simulation.RoadNetwork
    exposing
        ( ConnectionEnvironment(..)
        , ConnectionKind(..)
        , RNNodeContext
        )


uTurnDistance : Length
uTurnDistance =
    Length.meters 3


asGlobalSpline :
    Frame2d Length.Meters GlobalCoordinates { defines : LocalCoordinates }
    -> CubicSpline2d Length.Meters LocalCoordinates
    -> CubicSpline2d Length.Meters GlobalCoordinates
asGlobalSpline frame localSpline =
    CubicSpline2d.placeIn frame localSpline


type alias PathParameters =
    { origin : Point2d Length.Meters GlobalCoordinates
    , direction : Direction2d GlobalCoordinates
    , environment : ConnectionEnvironment
    }


toNode : PathParameters -> RNNodeContext -> CubicSpline2d Length.Meters GlobalCoordinates
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
                    if environment == Intersection then
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
            Common.rightAnglePoint origin target direction

        ( cp1, cp2 ) =
            ( Point2d.interpolateFrom origin rightAnglePos parameter
            , Point2d.interpolateFrom target rightAnglePos parameter
            )
    in
    CubicSpline2d.fromControlPoints origin cp1 cp2 target


type alias LotSplineProperties =
    { parkingSpotPosition : Point2d Length.Meters LocalCoordinates
    , lotEntryPoint : Point2d Length.Meters LocalCoordinates
    , lotExitPoint : Point2d Length.Meters LocalCoordinates
    , parkingLaneStartPoint : Point2d Length.Meters LocalCoordinates
    , parkingLaneStartDirection : Direction2d LocalCoordinates
    , parkingSpotExitDirection : Direction2d LocalCoordinates
    , entryDirection : Direction2d LocalCoordinates
    }


lotEntrySpline : LotSplineProperties -> List (CubicSpline2d Length.Meters LocalCoordinates)
lotEntrySpline { parkingSpotPosition, lotEntryPoint, parkingSpotExitDirection, parkingLaneStartPoint, parkingLaneStartDirection, entryDirection } =
    if parkingSpotExitDirection == Direction2d.reverse entryDirection && entryDirection == parkingLaneStartDirection then
        [ mirroredSpline lotEntryPoint parkingSpotPosition 0.5 entryDirection ]

    else if entryDirection /= parkingLaneStartDirection then
        [ curveSpline lotEntryPoint parkingLaneStartPoint entryDirection 0.7
        , if parkingLaneStartDirection == Direction2d.reverse parkingSpotExitDirection then
            straightSpline parkingLaneStartPoint parkingSpotPosition

          else
            curveSpline parkingLaneStartPoint parkingSpotPosition parkingLaneStartDirection 0.8
        ]

    else if parkingSpotCloseToLotEntry lotEntryPoint parkingSpotPosition then
        [ mirroredSpline lotEntryPoint parkingLaneStartPoint 0.33 entryDirection
        , curveSpline parkingLaneStartPoint parkingSpotPosition entryDirection 0.8
        ]

    else
        let
            parkingSpotSplineStart =
                parkingSpotSplineStartPoint
                    parkingLaneStartPoint
                    parkingSpotPosition
                    entryDirection
        in
        [ mirroredSpline lotEntryPoint parkingLaneStartPoint 0.33 entryDirection
        , straightSpline parkingLaneStartPoint parkingSpotSplineStart
        , curveSpline parkingSpotSplineStart parkingSpotPosition entryDirection 0.7
        ]


lotExitSpline : LotSplineProperties -> List (CubicSpline2d Length.Meters LocalCoordinates)
lotExitSpline { parkingSpotPosition, lotExitPoint, parkingSpotExitDirection, parkingLaneStartPoint, parkingLaneStartDirection, entryDirection } =
    let
        startDirection =
            parkingSpotExitDirection
    in
    if startDirection == Direction2d.reverse entryDirection && entryDirection == parkingLaneStartDirection then
        [ mirroredSpline parkingSpotPosition lotExitPoint 0.66 startDirection ]

    else if entryDirection /= parkingLaneStartDirection then
        [ if parkingLaneStartDirection == Direction2d.reverse startDirection then
            straightSpline parkingSpotPosition parkingLaneStartPoint

          else
            curveSpline parkingSpotPosition parkingLaneStartPoint startDirection 0.8
        , curveSpline parkingLaneStartPoint lotExitPoint (Direction2d.reverse parkingLaneStartDirection) 0.8
        ]

    else
        let
            exitDirection =
                Direction2d.reverse entryDirection
        in
        if parkingSpotCloseToLotEntry lotExitPoint parkingSpotPosition then
            [ curveSpline parkingSpotPosition parkingLaneStartPoint startDirection 0.8
            , mirroredSpline parkingLaneStartPoint lotExitPoint 0.33 exitDirection
            ]

        else
            let
                parkingSpotSplineStart =
                    parkingSpotSplineStartPoint
                        parkingLaneStartPoint
                        parkingSpotPosition
                        entryDirection
            in
            [ curveSpline parkingSpotPosition parkingSpotSplineStart startDirection 0.7
            , straightSpline parkingSpotSplineStart parkingLaneStartPoint
            , mirroredSpline parkingLaneStartPoint lotExitPoint 0.33 exitDirection
            ]


parkingSpotSplineStartPoint : Point2d Length.Meters a -> Point2d Length.Meters a -> Direction2d a -> Point2d Length.Meters a
parkingSpotSplineStartPoint parkingLaneStartPoint parkingSpotPosition startDirection =
    let
        xDiff =
            Point2d.xCoordinate parkingLaneStartPoint
                |> Quantity.minus (Point2d.xCoordinate parkingSpotPosition)
                |> Quantity.abs

        yDiff =
            Point2d.yCoordinate parkingLaneStartPoint
                |> Quantity.minus (Point2d.yCoordinate parkingSpotPosition)
                |> Quantity.abs

        distanceToParkingSpotSplineStart =
            xDiff |> Quantity.minus yDiff
    in
    parkingLaneStartPoint |> Point2d.translateIn startDirection distanceToParkingSpotSplineStart


parkingSpotCloseToLotEntry : Point2d Length.Meters a -> Point2d Length.Meters a -> Bool
parkingSpotCloseToLotEntry lotEntryOrExit parkingSpotPosition =
    Point2d.distanceFrom lotEntryOrExit parkingSpotPosition |> Quantity.lessThan (Length.meters 16)
