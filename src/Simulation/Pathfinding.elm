module Simulation.Pathfinding exposing
    ( carAfterRouteUpdate
    , resetCarAtLot
    , restoreRoute
    , setupRoute
    )

import Array
import BoundingBox2d
import Dict
import Duration exposing (Duration)
import Length
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Lot as Lot exposing (ParkingReservation)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.Route as Route exposing (Route)
import Model.World exposing (World)
import QuadTree
import Quantity
import Random
import Speed exposing (Speed)


setupRoute : Random.Seed -> World -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute seed world nodeCtx car =
    let
        ( route, nextSeed ) =
            Route.randomFromNode seed world.lots world.roadNetwork nodeCtx
    in
    ( { car | route = route }, nextSeed )


resetCarAtLot : Random.Seed -> ParkingReservation -> World -> RNNodeContext -> Car -> Car
resetCarAtLot seed parkingReservation world nodeCtx car =
    { car
        | route =
            Route.randomFromParkedAtLot
                seed
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
            (Route.randomFromParkedAtLot seed
                parkingReservation
                world.lots
                world.roadNetwork
            )
        |> Maybe.withDefault Route.initialRoute


type RouteUpdateResult
    = BeginRoute
    | ReachEndNode RNNodeContext Route
    | ArrivedAtDestination
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

                                    newRoute =
                                        Route.arriveToDestination parkingReservation world.lots
                                in
                                Car.triggerParking car parkingReservation newRoute
                            )
                        |> Maybe.withDefaultLazy (\_ -> Car.triggerWaitingForParking car nextRoute)

                _ ->
                    setupRoute seed world nodeCtx car |> Tuple.first

        ArrivedAtDestination ->
            let
                timerGenerator =
                    Random.float 1500 30000 |> Random.map Duration.milliseconds

                ( waitTimer, _ ) =
                    Random.step timerGenerator seed
            in
            { car | route = Route.Unrouted (Just waitTimer) }


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

        Route.ArrivingToDestination path ->
            let
                nextPath =
                    updatePath car.velocity delta path
            in
            if nextPath.finished then
                ArrivedAtDestination

            else
                Updated (Route.ArrivingToDestination nextPath)


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


restoreRoute : World -> Car -> Car
restoreRoute world car =
    if Car.isPathfinding car then
        car
        -- TODO!!!
        -- case
        --     Route.nextNode car.route
        --         |> Maybe.andThen (findNodeReplacement world)
        --         |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
        -- of
        --     Just nodeCtxResult ->
        --         { car
        --             | route =
        --                 Route.fromNode nodeCtxResult
        --                     car.position
        --                     (Direction2d.fromAngle car.orientation)
        --                     (Route.parking car.route)
        --         }
        --     Nothing ->
        --         Car.triggerDespawn car

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
