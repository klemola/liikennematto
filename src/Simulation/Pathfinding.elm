module Simulation.Pathfinding exposing
    ( carAfterRouteUpdate
    , resetCarAtLot
    , restoreRoute
    , routeTrafficControl
    , setupRoute
    , validateRoute
    )

import Array
import Dict
import Duration exposing (Duration)
import Length
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.Lot as Lot exposing (ParkingReservation)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( ConnectionKind(..)
        , RNNodeContext
        , TrafficControl
        )
import Model.Route as Route exposing (Route)
import Model.World as World exposing (World)
import Quantity
import Random
import Result.Extra as Result
import Speed exposing (Speed)


setupRoute : Random.Seed -> World -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute seed world nodeCtx car =
    let
        ( route, nextSeed ) =
            Route.randomFromNode seed 10 world.roadNetwork nodeCtx
    in
    ( { car | route = route }, nextSeed )


resetCarAtLot : Random.Seed -> ParkingReservation -> World -> RNNodeContext -> Car -> Car
resetCarAtLot seed parkingReservation world nodeCtx car =
    { car
        | route =
            Route.randomFromParkedAtLot
                seed
                10
                parkingReservation
                world.lots
                world.roadNetwork
                nodeCtx
    }


generateRouteFromParkingSpot : Random.Seed -> World -> ParkingReservation -> Route
generateRouteFromParkingSpot seed world parkingReservation =
    let
        lotExitNode =
            RoadNetwork.findLotExitNodeByLotId world.roadNetwork parkingReservation.lotId
    in
    lotExitNode
        |> Maybe.map
            (Route.randomFromParkedAtLot
                seed
                10
                parkingReservation
                world.lots
                world.roadNetwork
            )
        |> Maybe.withDefault Route.initialRoute


type RouteUpdateResult
    = BeginRoute
    | ReachEndNode RNNodeContext Route
    | ArrivedAtParkingSpot
    | ArrivedAtNode
    | Updated Route


carAfterRouteUpdate : Random.Seed -> World -> Duration -> Car -> Car
carAfterRouteUpdate seed world delta car =
    case updateRoute delta car of
        Updated nextRoute ->
            { car | route = nextRoute }

        BeginRoute ->
            let
                route =
                    case car.parkingReservation of
                        Just parkingReservation ->
                            let
                                parkingLockSet =
                                    world.lots
                                        |> Dict.get parkingReservation.lotId
                                        |> Maybe.map Lot.hasParkingLockSet
                                        |> Maybe.withDefault False
                            in
                            if not parkingLockSet then
                                generateRouteFromParkingSpot seed world parkingReservation

                            else
                                Route.initialRoute

                        Nothing ->
                            Route.initialRoute
            in
            { car | route = route }

        ReachEndNode nodeCtx nextRoute ->
            case
                nodeCtx.node.label.kind
            of
                LotEntry lotId ->
                    world.lots
                        |> Dict.get lotId
                        |> Maybe.andThen (Lot.attemptParking car.id)
                        |> Maybe.map
                            (\parkingSpot ->
                                let
                                    parkingReservation =
                                        { lotId = lotId
                                        , parkingSpotId = parkingSpot.id
                                        }
                                in
                                case Route.arriveToParkingSpot parkingReservation world.lots of
                                    Just newRoute ->
                                        Car.triggerParking car parkingReservation newRoute

                                    Nothing ->
                                        Car.triggerDespawn car
                            )
                        |> Maybe.withDefaultLazy (\_ -> Car.triggerWaitingForParking car nextRoute)

                _ ->
                    setupRoute seed world nodeCtx car |> Tuple.first

        ArrivedAtParkingSpot ->
            let
                timerGenerator =
                    Random.float 1500 30000 |> Random.map Duration.milliseconds

                ( waitTimer, _ ) =
                    Random.step timerGenerator seed
            in
            { car | route = Route.Unrouted (Just waitTimer) }

        ArrivedAtNode ->
            Car.triggerDespawn car


updateRoute : Duration -> Car -> RouteUpdateResult
updateRoute delta car =
    case car.route of
        Route.Unrouted timer ->
            let
                nextTimer =
                    timer |> Maybe.andThen (updateTimer delta)
            in
            case nextTimer of
                Nothing ->
                    BeginRoute

                _ ->
                    Updated (Route.Unrouted nextTimer)

        Route.Routed routeMeta ->
            let
                nextPath =
                    updatePath car.velocity delta routeMeta.path

                nextRoute =
                    Route.Routed { routeMeta | path = nextPath }
            in
            if nextPath.finished then
                ReachEndNode routeMeta.endNode nextRoute

            else
                Updated nextRoute

        Route.ArrivingToDestination destination path ->
            let
                nextPath =
                    updatePath car.velocity delta path
            in
            if nextPath.finished then
                case destination of
                    Route.LotParkingSpot ->
                        ArrivedAtParkingSpot

                    Route.RoadNetworkNode ->
                        ArrivedAtNode

            else
                Updated (Route.ArrivingToDestination destination nextPath)


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


updatePath : Speed -> Duration -> Route.Path -> Route.Path
updatePath velocity delta path =
    let
        currentSplineLength =
            path.currentSpline
                |> Maybe.map .length
                |> Maybe.withDefault Quantity.zero

        ( nextSplineIdx, nextSplineMeta, nextParameter ) =
            if Quantity.ratio path.parameter currentSplineLength >= 0.99 then
                let
                    idxPlusOne =
                        path.currentSplineIdx + 1
                in
                ( idxPlusOne
                , Array.get idxPlusOne path.splines
                  -- if the parameter overflows (more than the spline length), add the remainder to the next spline's parameter
                , path.parameter
                    |> Quantity.minus currentSplineLength
                    |> Quantity.max Quantity.zero
                )

            else
                ( path.currentSplineIdx
                , path.currentSpline
                , updateParameter velocity delta path.parameter
                )
    in
    { path
        | parameter = nextParameter
        , currentSplineIdx = nextSplineIdx
        , currentSpline = nextSplineMeta
        , finished = nextSplineMeta == Nothing
    }


updateParameter : Speed -> Duration -> Length.Length -> Length.Length
updateParameter velocity delta parameter =
    let
        deltaMeters =
            velocity |> Quantity.for delta
    in
    parameter |> Quantity.plus deltaMeters


routeTrafficControl : World -> Route -> Maybe ( TrafficControl, LMPoint2d )
routeTrafficControl world route =
    route
        |> Route.splineEndPoint
        |> Maybe.andThen (World.findNodeByPosition world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    trafficControlResult =
                        RoadNetwork.trafficControl nodeCtx
                in
                ( trafficControlResult, nodeCtx.node.label.position )
            )


restoreRoute : World -> Car -> Car
restoreRoute world car =
    if Route.isRouted car.route then
        let
            validationResult =
                validateEndNode world car.route
                    |> Result.map (\endNode -> Route.updateEndNode endNode car.route)
                    |> Result.map
                        (\routeWithValidEndNode ->
                            validateRoute world routeWithValidEndNode
                                -- Create a Maybe value of the valid route
                                |> Result.map Just
                                -- Try to recover from a partially invalid route
                                |> Result.extract
                                    (\( _, validNodes ) ->
                                        Maybe.andThen2
                                            (Route.fromPartialRoute car.route)
                                            (List.head validNodes)
                                            (List.tail validNodes)
                                    )
                        )
                    |> Result.withDefault (Route.stopAtSplineEnd car.route)
        in
        case validationResult of
            Just nextRoute ->
                { car | route = nextRoute }

            Nothing ->
                Car.triggerDespawn car

    else
        car


validateEndNode : World -> Route -> Result String RNNodeContext
validateEndNode world route =
    Route.endNode route
        |> Maybe.andThen
            (\endNode ->
                World.findNodeByPosition world endNode.node.label.position
                    |> Maybe.map
                        (\nodeByPosition ->
                            if nodeByPosition.node.label.kind == endNode.node.label.kind then
                                Ok nodeByPosition

                            else
                                Err "Invalid end node"
                        )
            )
        |> Maybe.withDefault (Err "End node not found")


validateRoute : World -> Route -> Result ( String, List RNNodeContext ) Route
validateRoute world route =
    validateRouteHelper world Nothing (Route.remainingNodePositions route) []
        |> Result.map (always route)


validateRouteHelper :
    World
    -> Maybe RNNodeContext
    -> List LMPoint2d
    -> List RNNodeContext
    -> Result ( String, List RNNodeContext ) (List RNNodeContext)
validateRouteHelper world previousNodeCtx remainingEndPoints validNodes =
    case remainingEndPoints of
        [] ->
            Ok validNodes

        endPoint :: others ->
            case World.findNodeByPosition world endPoint of
                Just currentNodeCtx ->
                    if
                        previousNodeCtx
                            |> Maybe.map (isDisconnectedFrom currentNodeCtx)
                            |> Maybe.withDefault False
                    then
                        Err
                            ( "Invalid route: found a disconnected pair of nodes"
                            , validNodes
                            )

                    else
                        validateRouteHelper
                            world
                            (Just currentNodeCtx)
                            others
                            (validNodes ++ [ currentNodeCtx ])

                Nothing ->
                    let
                        nodeId =
                            case previousNodeCtx of
                                Just nodeCtx ->
                                    String.fromInt nodeCtx.node.id

                                Nothing ->
                                    "none"
                    in
                    Err
                        ( String.join " "
                            [ "Invalid route: node not found. Last matching node id:"
                            , nodeId
                            ]
                        , validNodes
                        )


isDisconnectedFrom : RNNodeContext -> RNNodeContext -> Bool
isDisconnectedFrom currentNodeCtx previousNodeCtx =
    previousNodeCtx
        |> RoadNetwork.getOutgoingConnections
        |> List.member currentNodeCtx.node.id
        |> not
