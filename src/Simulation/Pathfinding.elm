module Simulation.Pathfinding exposing
    ( clearRoute
    , leaveLot
    , resetCarAtLot
    , restoreRoute
    , setupRoute
    , updateRoute
    )

import Array
import BoundingBox2d
import Dict
import Direction2d
import Duration exposing (Duration)
import Length
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (orthogonalDirectionToLmDirection)
import Model.Lot as Lot
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.Route as Route exposing (Route)
import Model.World exposing (World)
import QuadTree
import Quantity
import Random
import Random.List
import Speed exposing (Speed)


setupRoute : World -> Random.Seed -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute world seed nodeCtx car =
    let
        ( route, nextSeed ) =
            generateRouteFromConnection world car seed nodeCtx
    in
    ( { car | route = route }, nextSeed )


clearRoute : Car -> Car
clearRoute car =
    { car | route = Route.Unrouted }


resetCarAtLot : Route.Parking -> RNNodeContext -> Lot.ParkingSpot -> Car -> Car
resetCarAtLot parking nodeCtx parkingSpot car =
    { car
        | route =
            Route.fromLotExit
                parking
                nodeCtx
                parkingSpot
    }


leaveLot : World -> Car -> Car
leaveLot world car =
    let
        fallback =
            { car | route = Route.Unrouted }
    in
    case car.route of
        Route.Parked parking ->
            Dict.get parking.lotId world.lots
                |> Maybe.andThen
                    (\lot ->
                        let
                            parkingSpot =
                                Lot.parkingSpotById lot parking.parkingSpotId

                            lotExitNode =
                                RoadNetwork.findLotExitNodeByLotId world.roadNetwork parking.lotId
                        in
                        Maybe.map2 (Route.fromLotExit parking) lotExitNode parkingSpot
                            |> Maybe.map
                                (\route ->
                                    { car
                                        | orientation =
                                            lot.parkingSpotExitDirection
                                                |> orthogonalDirectionToLmDirection
                                                |> Direction2d.toAngle
                                        , route = route
                                    }
                                )
                    )
                |> Maybe.withDefault fallback

        _ ->
            fallback


updateRoute : World -> Duration -> Random.Seed -> Car -> ( Route, Random.Seed )
updateRoute world delta seed car =
    case car.route of
        Route.Unrouted ->
            ( car.route, seed )

        Route.Parked p ->
            let
                nextParking =
                    updateParking delta world car p
            in
            ( Route.Parked nextParking, seed )

        Route.Routed meta ->
            let
                nextMeta =
                    updateRouteMeta car.velocity delta meta
            in
            case nextMeta.currentSpline of
                Just _ ->
                    let
                        nextParking =
                            nextMeta.parking |> Maybe.map (updateParking delta world car)
                    in
                    ( Route.Routed { nextMeta | parking = nextParking }
                    , seed
                    )

                Nothing ->
                    case List.head nextMeta.connections of
                        Just currentNodeCtx ->
                            case currentNodeCtx.node.label.kind of
                                LotEntry _ ->
                                    -- TODO: parking should not be wrapped in Maybe here
                                    ( meta.parking
                                        |> Maybe.map Route.Parked
                                        |> Maybe.withDefault Route.Unrouted
                                    , seed
                                    )

                                _ ->
                                    generateRouteFromConnection world car seed currentNodeCtx

                        Nothing ->
                            ( Route.Unrouted, seed )


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


updateRouteMeta : Speed -> Duration -> Route.RouteMeta -> Route.RouteMeta
updateRouteMeta velocity delta meta =
    let
        ( nextSplineIdx, nextSplineMeta, nextParameter ) =
            if meta.parameter >= 0.99 then
                let
                    idxPlusOne =
                        meta.currentSplineIdx + 1
                in
                ( idxPlusOne
                , Array.get idxPlusOne meta.path
                  -- if the parameter overflows (e.g. it is 1.05), add the remainder to the next spline's parameter
                , max 0 (meta.parameter - 1)
                )

            else
                ( meta.currentSplineIdx
                , meta.currentSpline
                , case meta.currentSpline of
                    Just spline ->
                        updateParameter velocity delta spline.length meta.parameter

                    Nothing ->
                        meta.parameter
                )
    in
    { meta
        | parameter = nextParameter
        , currentSplineIdx = nextSplineIdx
        , currentSpline = nextSplineMeta
    }


updateParameter : Speed -> Duration -> Length.Length -> Float -> Float
updateParameter velocity delta length parameter =
    let
        deltaMeters =
            velocity |> Quantity.for delta

        parameterDelta =
            Quantity.ratio deltaMeters length
    in
    parameter + parameterDelta


generateRouteFromConnection : World -> Car -> Random.Seed -> RNNodeContext -> ( Route, Random.Seed )
generateRouteFromConnection world car seed currentNodeCtx =
    let
        randomConnectionGenerator =
            chooseRandomOutgoingConnection world car currentNodeCtx

        ( connection, nextSeed ) =
            Random.step randomConnectionGenerator seed
    in
    case connection of
        Just nextNodeCtx ->
            case nextNodeCtx.node.label.kind of
                LotEntry lotId ->
                    let
                        route =
                            Dict.get lotId world.lots
                                |> Maybe.andThen (Lot.findFreeParkingSpot car.id)
                                |> Maybe.map
                                    (\parkingSpot ->
                                        let
                                            timerGenerator =
                                                Random.float 1500 30000 |> Random.map Duration.milliseconds

                                            ( waitTimer, _ ) =
                                                Random.step timerGenerator seed

                                            parking =
                                                { lotId = lotId
                                                , parkingSpotId = parkingSpot.id
                                                , waitTimer = Just waitTimer
                                                , lockAvailable = True
                                                }
                                        in
                                        Route.fromParkingSpot
                                            nextNodeCtx
                                            car.position
                                            (Direction2d.fromAngle car.orientation)
                                            parking
                                            parkingSpot.pathFromLotEntry
                                    )
                                |> Maybe.withDefault Route.Unrouted
                    in
                    ( route, nextSeed )

                _ ->
                    ( Route.fromNode
                        nextNodeCtx
                        car.position
                        currentNodeCtx.node.label.direction
                        (Route.parking car.route)
                    , nextSeed
                    )

        Nothing ->
            ( Route.Unrouted, seed )


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
            Route.nextNode car.route
                |> Maybe.andThen (findNodeReplacement world)
                |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
        of
            Just nodeCtxResult ->
                { car
                    | route =
                        Route.fromNode nodeCtxResult
                            car.position
                            (Direction2d.fromAngle car.orientation)
                            (Route.parking car.route)
                }

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
