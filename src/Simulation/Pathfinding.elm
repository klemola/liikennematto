module Simulation.Pathfinding exposing
    ( leaveLot
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
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext, RoadNetwork)
import Model.Route as Route exposing (Route)
import Model.World exposing (World)
import QuadTree
import Quantity
import Random
import Speed exposing (Speed)


setupRoute : World -> Random.Seed -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute world seed nodeCtx car =
    let
        ( route, nextSeed ) =
            Route.randomFromNode seed world.lots world.roadNetwork nodeCtx
    in
    ( { car | route = route }, nextSeed )


resetCarAtLot : Random.Seed -> Route.Parking -> World -> RNNodeContext -> Car -> Car
resetCarAtLot seed parking world nodeCtx car =
    { car
        | route =
            Route.randomFromParkedAtLot
                seed
                parking
                world.lots
                world.roadNetwork
                nodeCtx
    }


leaveLot : Random.Seed -> World -> Car -> Car
leaveLot seed world car =
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
                            lotExitNode =
                                RoadNetwork.findLotExitNodeByLotId world.roadNetwork parking.lotId
                        in
                        lotExitNode
                            |> Maybe.map
                                (Route.randomFromParkedAtLot seed parking world.lots world.roadNetwork)
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


updateRoute : World -> Duration -> Car -> Route
updateRoute world delta car =
    case car.route of
        Route.Unrouted ->
            car.route

        Route.Parked p ->
            let
                nextParking =
                    updateParking delta world car p
            in
            Route.Parked nextParking

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
                    Route.Routed { nextMeta | parking = nextParking }

                Nothing ->
                    case nextMeta.endNode.node.label.kind of
                        LotEntry _ ->
                            -- TODO: parking should not be wrapped in Maybe here
                            meta.parking
                                |> Maybe.map Route.Parked
                                |> Maybe.withDefault Route.Unrouted

                        _ ->
                            Route.Unrouted


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
        currentSplineLength =
            meta.currentSpline
                |> Maybe.map .length
                |> Maybe.withDefault Quantity.zero

        ( nextSplineIdx, nextSplineMeta, nextParameter ) =
            if Quantity.ratio meta.parameter currentSplineLength >= 0.99 then
                let
                    idxPlusOne =
                        meta.currentSplineIdx + 1
                in
                ( idxPlusOne
                , Array.get idxPlusOne meta.path
                  -- if the parameter overflows (more than the spline length), add the remainder to the next spline's parameter
                , meta.parameter
                    |> Quantity.minus currentSplineLength
                    |> Quantity.max Quantity.zero
                )

            else
                ( meta.currentSplineIdx
                , meta.currentSpline
                , updateParameter velocity delta meta.parameter
                )
    in
    { meta
        | parameter = nextParameter
        , currentSplineIdx = nextSplineIdx
        , currentSpline = nextSplineMeta
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
