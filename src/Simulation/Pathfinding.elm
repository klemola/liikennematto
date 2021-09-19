module Simulation.Pathfinding exposing (createRoute, maybeCreateRoute)

import Angle
import Common
import CubicSpline2d exposing (CubicSpline2d)
import Direction2d
import Length exposing (Length)
import Model.Car exposing (Car, Status(..))
import Model.Geometry exposing (LMDirection2d, LMEntityCoordinates, LMPoint2d)
import Model.LocalPath exposing (LocalPath)
import Model.RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Point2d
import Polyline2d
import Quantity


type alias PathParameters =
    { origin : LMPoint2d
    , direction : LMDirection2d
    , useOffsetSpline : Bool
    }


splineSegmentsAmount : Int
splineSegmentsAmount =
    20


lotExitOffset : Length
lotExitOffset =
    Length.meters 10


uTurnDistance : Length
uTurnDistance =
    Length.meters 4


maybeCreateRoute : Maybe RNNodeContext -> Car -> Car
maybeCreateRoute maybeNodeCtx car =
    maybeNodeCtx
        |> Maybe.map (\nodeCtx -> createRoute nodeCtx car)
        |> Maybe.withDefault car


createRoute : RNNodeContext -> Car -> Car
createRoute nodeCtx car =
    let
        newPathRequired =
            case car.route of
                target :: _ ->
                    target.node.label.position /= nodeCtx.node.label.position

                _ ->
                    True
    in
    { car
        | route = [ nodeCtx ]
        , localPath =
            if newPathRequired then
                toNode
                    { origin = car.position
                    , direction = Direction2d.fromAngle car.orientation
                    , useOffsetSpline = car.status == ParkedAtLot
                    }
                    nodeCtx

            else
                car.localPath
    }


toNode : PathParameters -> RNNodeContext -> LocalPath
toNode { direction, origin, useOffsetSpline } { node } =
    let
        target =
            node.label.position

        angleDegreesToTarget =
            origin
                |> Common.angleFromDirection direction target
                |> Quantity.abs
    in
    if node.label.kind == LotEntry && useOffsetSpline then
        offsetSpline origin target direction

    else if node.label.kind == DeadendExit then
        uTurnSpline origin target direction

    else if angleDegreesToTarget |> Quantity.lessThan (Angle.radians 0.1) then
        [ origin, target ]

    else
        curveSpline origin target direction


offsetSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
offsetSpline origin target direction =
    let
        targetCp =
            target |> Point2d.translateIn direction lotExitOffset

        distanceToTarget =
            Point2d.distanceFrom origin targetCp

        handleCp1 =
            origin
                |> Point2d.translateIn
                    direction
                    (distanceToTarget |> Quantity.multiplyBy 0.25)

        handleCp2 =
            targetCp
                |> Point2d.translateIn
                    (Direction2d.reverse direction)
                    (distanceToTarget |> Quantity.half)
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 targetCp
        |> cubicSplineToLocalPath


uTurnSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
uTurnSpline origin target direction =
    let
        handleCp1 =
            Point2d.translateIn direction uTurnDistance origin

        handleCp2 =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> cubicSplineToLocalPath


curveSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LocalPath
curveSpline origin target direction =
    let
        distanceToTarget =
            Point2d.distanceFrom origin target

        cosine =
            origin
                |> Common.angleFromDirection direction target
                |> Angle.cos

        distanceToCorner =
            Quantity.multiplyBy cosine distanceToTarget

        corner =
            Point2d.translateIn direction distanceToCorner origin

        handleCp1 =
            Point2d.midpoint origin corner

        handleCp2 =
            Point2d.midpoint corner target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> cubicSplineToLocalPath


cubicSplineToLocalPath : CubicSpline2d Length.Meters LMEntityCoordinates -> LocalPath
cubicSplineToLocalPath spline =
    spline
        |> CubicSpline2d.segments splineSegmentsAmount
        |> Polyline2d.vertices
        |> List.tail
        |> Maybe.withDefault []
