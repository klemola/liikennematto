module Simulation.Pathfinding exposing
    ( chooseRandomOutgoingConnection
    , createRouteToLotExit
    , createRouteToNode
    , maybeCreateRouteToNode
    , restoreRoute
    , updatePath
    )

import BoundingBox2d
import CubicSpline2d
import Direction2d
import Length
import Model.Car as Car exposing (Car, CarState(..))
import Model.Geometry exposing (LMCubicSpline2d, LMPolyline2d)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext, RoadNetwork)
import Model.World exposing (World)
import Point2d
import Polyline2d
import QuadTree
import Random
import Random.List
import Splines


splineSegmentsAmount : Int
splineSegmentsAmount =
    20


cubicSplineToLocalPath : LMCubicSpline2d -> LMPolyline2d
cubicSplineToLocalPath spline =
    CubicSpline2d.segments splineSegmentsAmount spline


multipleSplinesToLocalPath : List LMCubicSpline2d -> LMPolyline2d
multipleSplinesToLocalPath splines =
    splines
        |> List.concatMap (CubicSpline2d.segments splineSegmentsAmount >> Polyline2d.vertices)
        |> Polyline2d.fromVertices


maybeCreateRouteToNode : Maybe RNNodeContext -> Car -> Car
maybeCreateRouteToNode maybeNodeCtx car =
    maybeNodeCtx
        |> Maybe.map (\nodeCtx -> createRouteToNode nodeCtx car)
        |> Maybe.withDefault car


createRouteToNode : RNNodeContext -> Car -> Car
createRouteToNode nodeCtx car =
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
                nodeCtx
                    |> Splines.toNode
                        { origin = car.position
                        , direction = Direction2d.fromAngle car.orientation
                        }
                    |> cubicSplineToLocalPath

            else
                car.localPath
    }


createRouteToLotExit : RNNodeContext -> List LMCubicSpline2d -> Car -> Car
createRouteToLotExit nodeCtx pathToLotExit car =
    { car
        | route = [ nodeCtx ]
        , localPath = multipleSplinesToLocalPath pathToLotExit
    }


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
                    chooseRandomOutgoingConnection world.roadNetwork nodeCtx

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
                            createRouteToNode nextNodeCtx adjusted

                        Nothing ->
                            car
            in
            ( nextCar, nextSeed )

        _ ->
            ( Car.triggerReroute car, seed )


chooseRandomOutgoingConnection : RoadNetwork -> RNNodeContext -> Random.Generator (Maybe RNNodeContext)
chooseRandomOutgoingConnection roadNetwork nodeCtx =
    RoadNetwork.getOutgoingConnections nodeCtx
        |> List.filterMap (RoadNetwork.findNodeByNodeId roadNetwork >> Maybe.andThen discardLotNodes)
        |> Random.List.choose
        |> Random.map Tuple.first


discardLotNodes : RNNodeContext -> Maybe RNNodeContext
discardLotNodes nodeCtx =
    case nodeCtx.node.label.kind of
        LotEntry _ ->
            Nothing

        LotExit _ ->
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
                createRouteToNode nodeCtxResult car

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
