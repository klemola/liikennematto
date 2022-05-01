module Simulation.Pathfinding exposing
    ( createRouteToLotExit
    , createRouteToNode
    , initRoute
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
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMCubicSpline2d, LMPolyline2d)
import Model.Lot as Lot exposing (ParkingSpot)
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


createRouteToNode : RNNodeContext -> Car -> Car
createRouteToNode nodeCtx car =
    let
        currentRoute =
            car.route

        newPathRequired =
            case currentRoute.connections of
                target :: _ ->
                    target.node.label.position /= nodeCtx.node.label.position

                _ ->
                    True
    in
    { car
        | route = { currentRoute | connections = [ nodeCtx ] }
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
        | route =
            { connections = [ nodeCtx ]
            , parking = Nothing
            }
        , localPath = multipleSplinesToLocalPath pathToLotExit
    }


createRouteToParkingSpot : Id -> RNNodeContext -> ParkingSpot -> Car -> Car
createRouteToParkingSpot lotId lotEntryNode parkingSpot car =
    let
        pathToLotEntry =
            Splines.toNode
                { origin = car.position
                , direction = Direction2d.fromAngle car.orientation
                }
                lotEntryNode
    in
    { car
        | route =
            { connections = []
            , parking =
                Just
                    { lotId = lotId
                    , parkingSpotId = parkingSpot.id
                    }
            }
        , localPath = multipleSplinesToLocalPath (pathToLotEntry :: parkingSpot.pathFromLotEntry)
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
            case List.head car.route.connections of
                Just currentNodeCtx ->
                    case currentNodeCtx.node.label.kind of
                        LotEntry _ ->
                            -- Having no route is expected once the parking routine has started
                            let
                                nextCar =
                                    if Car.isParking car then
                                        car

                                    else
                                        Car.triggerReroute car
                            in
                            ( nextCar, seed )

                        otherKind ->
                            let
                                adjustedCar =
                                    -- This is a temporary hack to make sure that tight turns can be completed
                                    if otherKind == DeadendExit || otherKind == LaneConnector then
                                        { car
                                            | orientation = Direction2d.toAngle currentNodeCtx.node.label.direction
                                            , position = currentNodeCtx.node.label.position
                                        }

                                    else
                                        car
                            in
                            generateRouteFromConnection world adjustedCar seed currentNodeCtx
                                |> Maybe.withDefault ( Car.triggerReroute adjustedCar, seed )

                Nothing ->
                    if Car.isParking car then
                        ( car, seed )

                    else
                        ( Car.triggerReroute car, seed )


generateRouteFromConnection : World -> Car -> Random.Seed -> RNNodeContext -> Maybe ( Car, Random.Seed )
generateRouteFromConnection world car seed currentNodeCtx =
    let
        randomConnectionGenerator =
            chooseRandomOutgoingConnection world car currentNodeCtx

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed
    in
    Maybe.map
        (\nextNodeCtx ->
            let
                carWithRoute =
                    case nextNodeCtx.node.label.kind of
                        LotEntry lotId ->
                            Dict.get lotId world.lots
                                |> Maybe.andThen (Lot.findFreeParkingSpot car.id)
                                |> Maybe.map
                                    (\parkingSpot ->
                                        car
                                            |> createRouteToParkingSpot lotId nextNodeCtx parkingSpot
                                            |> Car.triggerParking
                                    )
                                |> Maybe.withDefault
                                    (Car.triggerReroute car)

                        _ ->
                            createRouteToNode nextNodeCtx car
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
                -- Given that a lot is found, continue only if the car has a home (e.g. the car is not a test car)
                -- Will be enabled once there are enough lots with residents
                -- |> Maybe.next car.homeLotId
                |> Maybe.andThen (Lot.findFreeParkingSpot car.id)
                |> Maybe.map (always nodeCtx)

        LotExit _ ->
            Nothing

        _ ->
            Just nodeCtx


restoreRoute : World -> Car -> Car
restoreRoute world car =
    if Car.isPathfinding car then
        case
            List.head car.route.connections
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
