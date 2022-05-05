module Simulation.Pathfinding exposing
    ( clearRoute
    , createRouteToLotExit
    , createRouteToNode
    , leaveLot
    , restoreRoute
    , setupParking
    , setupRoute
    , updateRoute
    )

import BoundingBox2d
import CubicSpline2d
import Dict
import Direction2d
import Duration exposing (Duration)
import Length
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (LMCubicSpline2d, LMPolyline2d, orthogonalDirectionToLmDirection)
import Model.Lot as Lot exposing (ParkingSpot)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.Route as Route
import Model.World exposing (World)
import Point2d
import Polyline2d
import QuadTree
import Quantity
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


setupRoute : World -> Random.Seed -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute world seed nodeCtx car =
    nodeCtx
        |> generateRouteFromConnection world car seed
        |> Maybe.withDefault ( Car.triggerDespawn car, seed )


clearRoute : Car -> Car
clearRoute car =
    { car | route = Route.unrouted }


setupParking : Route.Parking -> Car -> Car
setupParking parking car =
    let
        route =
            { connections = []
            , parking = Just parking
            }
    in
    { car | route = route }


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
        | route =
            { connections = [ nodeCtx ]
            , parking =
                if Car.isDriving car then
                    Nothing

                else
                    currentRoute.parking
            }
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
            , parking = car.route.parking
            }
        , localPath = multipleSplinesToLocalPath pathToLotExit
    }


createRouteToParkingSpot : Id -> World -> Car -> RNNodeContext -> Random.Seed -> ( Car, Random.Seed )
createRouteToParkingSpot lotId world car nextNodeCtx seed =
    let
        timerGenerator =
            Random.float 1500 30000 |> Random.map Duration.milliseconds

        ( waitTimer, nextSeed ) =
            Random.step timerGenerator seed

        nextCar =
            Dict.get lotId world.lots
                |> Maybe.andThen (Lot.findFreeParkingSpot car.id)
                |> Maybe.map
                    (\parkingSpot ->
                        car
                            |> setupParking
                                { lotId = lotId
                                , parkingSpotId = parkingSpot.id
                                , waitTimer = Just waitTimer
                                , lockAvailable = True
                                }
                            |> createPathToParkingSpot nextNodeCtx parkingSpot
                            |> Car.triggerParking
                    )
                |> Maybe.withDefault (Car.triggerDespawn car)
    in
    ( nextCar, nextSeed )


createPathToParkingSpot : RNNodeContext -> ParkingSpot -> Car -> Car
createPathToParkingSpot lotEntryNode parkingSpot car =
    let
        pathToLotEntry =
            Splines.toNode
                { origin = car.position
                , direction = Direction2d.fromAngle car.orientation
                }
                lotEntryNode
    in
    { car
        | localPath = multipleSplinesToLocalPath (pathToLotEntry :: parkingSpot.pathFromLotEntry)
    }


leaveLot : World -> Car -> Car
leaveLot world car =
    case car.route.parking of
        Just { lotId, parkingSpotId } ->
            Dict.get lotId world.lots
                |> Maybe.andThen
                    (\lot ->
                        let
                            parkingSpot =
                                Lot.parkingSpotById lot parkingSpotId |> Maybe.map .pathToLotExit

                            lotExitNode =
                                RoadNetwork.findLotExitNodeByLotId world.roadNetwork lotId

                            rotatedCar =
                                Just
                                    { car
                                        | orientation =
                                            lot.parkingSpotExitDirection
                                                |> orthogonalDirectionToLmDirection
                                                |> Direction2d.toAngle
                                    }
                        in
                        Maybe.map3 createRouteToLotExit lotExitNode parkingSpot rotatedCar
                    )
                |> Maybe.withDefault car

        Nothing ->
            car


updateRoute : World -> Duration -> Random.Seed -> Car -> ( Car, Random.Seed )
updateRoute world delta seed car =
    let
        currentRoute =
            car.route

        routeWithParkingUpdate =
            { currentRoute
                | parking =
                    currentRoute.parking |> Maybe.map (updateParking delta world car)
            }

        updatedCar =
            { car | route = routeWithParkingUpdate }
    in
    case Polyline2d.vertices updatedCar.localPath of
        next :: others ->
            if Point2d.equalWithin (Length.meters 0.5) updatedCar.position next then
                ( { updatedCar | localPath = Polyline2d.fromVertices others }
                , seed
                )

            else
                ( updatedCar, seed )

        [] ->
            case List.head routeWithParkingUpdate.connections of
                Just currentNodeCtx ->
                    case currentNodeCtx.node.label.kind of
                        LotEntry _ ->
                            ( updatedCar, seed )

                        otherKind ->
                            let
                                adjustedCar =
                                    -- This is a temporary hack to make sure that tight turns can be completed
                                    if otherKind == DeadendExit || otherKind == LaneConnector then
                                        { updatedCar
                                            | orientation = Direction2d.toAngle currentNodeCtx.node.label.direction
                                            , position = currentNodeCtx.node.label.position
                                        }

                                    else
                                        updatedCar
                            in
                            generateRouteFromConnection world adjustedCar seed currentNodeCtx
                                |> Maybe.withDefault ( Car.triggerDespawn adjustedCar, seed )

                Nothing ->
                    ( updatedCar, seed )


updateParking : Duration -> World -> Car -> Route.Parking -> Route.Parking
updateParking delta world car parking =
    let
        nextTimer =
            if Car.isParked car then
                parking.waitTimer |> Maybe.andThen (updateTimer delta)

            else
                parking.waitTimer

        lockAvailable =
            Dict.get parking.lotId world.lots
                |> Maybe.map (Lot.hasParkingLockSet >> not)
                |> Maybe.withDefault False
    in
    { parking
        | waitTimer = nextTimer
        , lockAvailable = lockAvailable
    }


updateTimer : Duration -> Duration -> Maybe Duration
updateTimer delta timer =
    let
        nextDuration =
            timer |> Quantity.minus delta
    in
    if Quantity.lessThanOrEqualToZero nextDuration then
        Nothing

    else
        Just nextDuration


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
            case nextNodeCtx.node.label.kind of
                LotEntry lotId ->
                    createRouteToParkingSpot lotId world car nextNodeCtx nextSeed

                _ ->
                    ( createRouteToNode nextNodeCtx car, nextSeed )
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
                |> Maybe.filter (Lot.parkingAllowed car.id)
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
                Car.triggerDespawn car

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
