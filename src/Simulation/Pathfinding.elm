module Simulation.Pathfinding exposing
    ( chooseRandomOutgoingConnection
    , createRouteToLotExit
    , createRouteToNode
    , initRoute
    , maybeCreateRouteToNode
    , restoreRoute
    , updatePath
    )

import BoundingBox2d
import CubicSpline2d
import Dict
import Direction2d
import Length
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Geometry exposing (LMCubicSpline2d, LMPolyline2d)
import Model.Lot as Lot
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
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


initRoute : World -> Random.Seed -> RNNodeContext -> Car -> ( Car, Random.Seed )
initRoute world seed nodeCtx car =
    nodeCtx
        |> generateRouteFromConnection world car seed
        |> Maybe.withDefault ( Car.triggerReroute car, seed )


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
    car.route
        |> List.head
        |> Maybe.andThen (generateRouteFromConnection world car seed)
        |> Maybe.withDefault ( Car.triggerReroute car, seed )


generateRouteFromConnection : World -> Car -> Random.Seed -> RNNodeContext -> Maybe ( Car, Random.Seed )
generateRouteFromConnection world car seed nodeCtx =
    let
        randomConnectionGenerator =
            chooseRandomOutgoingConnection world car nodeCtx

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed
    in
    Maybe.map
        (\nextNodeCtx ->
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

                carWithRoute =
                    createRouteToNode nextNodeCtx adjusted
            in
            ( carWithRoute, nextSeed )
        )
        connection


chooseRandomOutgoingConnection : World -> Car -> RNNodeContext -> Random.Generator (Maybe RNNodeContext)
chooseRandomOutgoingConnection world car nodeCtx =
    RoadNetwork.getOutgoingConnections nodeCtx
        |> List.filterMap (RoadNetwork.findNodeByNodeId world.roadNetwork >> Maybe.andThen (discardInvalidConnections world car))
        |> Random.List.choose
        |> Random.map Tuple.first


discardInvalidConnections : World -> Car -> RNNodeContext -> Maybe RNNodeContext
discardInvalidConnections world car nodeCtx =
    case nodeCtx.node.label.kind of
        LotEntry lotId ->
            world.lots
                |> Dict.get lotId
                -- given that a lot is found, continue only if the car has a home
                |> Maybe.next car.homeLotId
                |> Maybe.map (Lot.findFreeParkingSpot car.id >> always nodeCtx)

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
