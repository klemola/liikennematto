module Simulation.Pathfinding exposing
    ( createRoute
    , maybeCreateRoute
    , restoreRoute
    , updatePath
    )

import Angle
import BoundingBox2d
import Common
import CubicSpline2d
import Direction2d
import Length exposing (Length)
import Model.Car as Car exposing (Car, CarState(..))
import Model.Geometry
    exposing
        ( LMCubicSpline2d
        , LMDirection2d
        , LMPoint2d
        , LMPolyline2d
        )
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.World exposing (World)
import Point2d
import Polyline2d
import QuadTree
import Quantity
import Random
import Random.List


type alias PathParameters =
    { origin : LMPoint2d
    , direction : LMDirection2d
    }


splineSegmentsAmount : Int
splineSegmentsAmount =
    20


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
                    }
                    nodeCtx

            else
                car.localPath
    }


toNode : PathParameters -> RNNodeContext -> LMPolyline2d
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
        Polyline2d.fromVertices [ origin, target ]

    else
        curveSpline origin target direction


uTurnSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LMPolyline2d
uTurnSpline origin target direction =
    let
        handleCp1 =
            Point2d.translateIn direction uTurnDistance origin

        handleCp2 =
            Point2d.translateIn direction uTurnDistance target
    in
    CubicSpline2d.fromControlPoints origin handleCp1 handleCp2 target
        |> cubicSplineToLocalPath


curveSpline : LMPoint2d -> LMPoint2d -> LMDirection2d -> LMPolyline2d
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


cubicSplineToLocalPath : LMCubicSpline2d -> LMPolyline2d
cubicSplineToLocalPath spline =
    CubicSpline2d.segments splineSegmentsAmount spline


updatePath : World -> Random.Seed -> Car -> ( Car, Random.Seed )
updatePath world seed car =
    case Polyline2d.vertices car.localPath of
        next :: others ->
            if Point2d.equalWithin (Length.meters 0.5) car.position next then
                ( { car | localPath = Polyline2d.fromVertices others }, seed )

            else
                ( car, seed )

        [] ->
            chooseNextConnection seed world car


chooseNextConnection : Random.Seed -> World -> Car -> ( Car, Random.Seed )
chooseNextConnection seed world car =
    case car.route of
        nodeCtx :: _ ->
            let
                randomConnectionGenerator =
                    RoadNetwork.getOutgoingConnections nodeCtx
                        |> List.filterMap (RoadNetwork.findNodeByNodeId world.roadNetwork >> Maybe.andThen discardLotEntry)
                        |> Random.List.choose
                        |> Random.map Tuple.first

                ( connection, nextSeed ) =
                    Random.step randomConnectionGenerator seed

                nextCar =
                    case connection of
                        Just nextNodeCtx ->
                            -- This is a temporary hack to make sure that tight turns can be completed
                            let
                                nodeKind =
                                    nodeCtx.node.label.kind

                                adjusted =
                                    if nodeKind == DeadendExit || nodeKind == LaneConnector then
                                        { car
                                            | orientation = Direction2d.toAngle nodeCtx.node.label.direction
                                            , position = nodeCtx.node.label.position
                                        }

                                    else
                                        car
                            in
                            createRoute nextNodeCtx adjusted

                        Nothing ->
                            car
            in
            ( nextCar, nextSeed )

        _ ->
            ( Car.triggerReroute car, seed )


discardLotEntry : RNNodeContext -> Maybe RNNodeContext
discardLotEntry nodeCtx =
    case nodeCtx.node.label.kind of
        LotEntry _ ->
            Nothing

        _ ->
            Just nodeCtx


restoreRoute : World -> Car -> Car
restoreRoute world car =
    if Car.isPathfinding car then
        case
            List.head car.route
                |> Maybe.andThen (findNodeReplacement world)
                |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
        of
            Just nodeCtxResult ->
                createRoute nodeCtxResult car

            Nothing ->
                Car.triggerReroute car

    else
        car


findNodeReplacement : World -> RNNodeContext -> Maybe Int
findNodeReplacement { roadNetworkLookup } target =
    -- Tries to find a node in the lookup tree that could replace the reference node.
    -- Useful in maintaning route after the road network has been updated.
    let
        targetPosition =
            target.node.label.position

        positionMatches =
            roadNetworkLookup
                |> QuadTree.findIntersecting
                    { id = target.node.id
                    , position = targetPosition
                    , boundingBox = BoundingBox2d.singleton targetPosition
                    }
    in
    case positionMatches of
        match :: _ ->
            Just match.id

        [] ->
            Nothing
