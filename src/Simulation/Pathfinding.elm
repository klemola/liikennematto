module Simulation.Pathfinding exposing
    ( clearRoute
    , leaveLot
    , resetCarAtLot
    , restoreRoute
    , setupRoute
    , updateRoute
    )

import BoundingBox2d
import Dict
import Direction2d
import Duration exposing (Duration)
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


setupRoute : World -> Random.Seed -> RNNodeContext -> Car -> ( Car, Random.Seed )
setupRoute world seed nodeCtx car =
    nodeCtx
        |> generateRouteFromConnection world car seed
        |> Maybe.withDefault ( Car.triggerDespawn car, seed )


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
    case Route.parking car.route of
        Just parking ->
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
                |> Maybe.withDefault car

        Nothing ->
            car


updateRoute : World -> Duration -> Random.Seed -> Car -> ( Route, Random.Seed )
updateRoute world delta seed car =
    case car.route of
        Route.Unrouted ->
            ( car.route, seed )

        Route.Parked p ->
            ( Route.Parked (updateParking delta world car p), seed )

        Route.Routed meta ->
            let
                parking =
                    meta.parking |> Maybe.map (updateParking delta world car)
            in
            -- Update the parameter and if the parameter is 1.0 then move to the next node (or generate new)
            -- case currentNodeCtx.node.label.kind of
            --     -- ..unless the current node is the lot where the car has parked.
            --     LotEntry _ ->
            --         ( updatedCar, seed )
            --     otherKind ->
            --         generateRouteFromConnection world adjustedCar seed currentNodeCtx
            --             |> Maybe.withDefault ( Car.triggerDespawn adjustedCar, seed )
            ( Route.Routed { meta | parking = parking }, seed )


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
                    let
                        nextCar =
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

                                            route =
                                                Route.fromParkingSpot
                                                    nextNodeCtx
                                                    car.position
                                                    (Direction2d.fromAngle car.orientation)
                                                    parking
                                                    parkingSpot.pathFromLotEntry
                                        in
                                        Car.triggerParking car route
                                    )
                                |> Maybe.withDefault (Car.triggerDespawn car)
                    in
                    ( nextCar, nextSeed )

                _ ->
                    ( { car
                        | route =
                            Route.fromNode
                                nextNodeCtx
                                car.position
                                (Direction2d.fromAngle car.orientation)
                                (Route.parking car.route)
                      }
                    , nextSeed
                    )
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
