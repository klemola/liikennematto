module Simulation.Pathfinding exposing
    ( attemptBeginParking
    , attemptGenerateRouteFromNode
    , attemptGenerateRouteFromParkingSpot
    , restoreRoute
    , routeTrafficControl
    , updateRoute
    )

import Array
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarRole(..))
import Duration exposing (Duration)
import Length exposing (Length)
import Lib.Collection as Collection exposing (Id)
import Model.World as World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Random
import Simulation.Car as Car exposing (Car)
import Simulation.Lot as Lot exposing (ParkingReservation, ParkingSpot)
import Simulation.RoadNetwork as RoadNetwork
    exposing
        ( ConnectionKind(..)
        , RNNode
        , RNNodeContext
        , TrafficControl
        )
import Simulation.Route as Route exposing (Route)
import Speed exposing (Speed)


minRouteEndNodeDistance : Length
minRouteEndNodeDistance =
    Length.meters 64


lotToNodeRouteTargetRatio : Float
lotToNodeRouteTargetRatio =
    2 / 3


updateRoute : Duration -> Car -> ( Route, World.WorldEvent )
updateRoute delta car =
    case car.route of
        Route.Unrouted ->
            ( car.route, World.None )

        Route.Routed routeMeta ->
            let
                nextPath =
                    updatePath car.velocity delta routeMeta.path
            in
            if nextPath.finished then
                let
                    worldEvent =
                        case
                            routeMeta.endNode.node.label.kind
                        of
                            LotEntry lotId ->
                                -- Arrived to a lot
                                if Car.currentState car == Car.Driving then
                                    World.BeginCarParking { carId = car.id, lotId = lotId }

                                else
                                    World.None

                            _ ->
                                -- Arrived to a node
                                World.CreateRouteFromNode car.id routeMeta.endNode
                in
                ( Route.Unrouted, worldEvent )

            else
                let
                    nextRoute =
                        Route.Routed { routeMeta | path = nextPath }
                in
                ( nextRoute, World.None )

        Route.ArrivingToDestination destination path ->
            let
                nextPath =
                    updatePath car.velocity delta path
            in
            if nextPath.finished then
                ( Route.Unrouted, World.None )

            else
                ( Route.ArrivingToDestination destination nextPath, World.None )


updatePath : Speed -> Duration -> Route.Path -> Route.Path
updatePath velocity delta path =
    let
        currentSplineLength =
            path.currentSpline
                |> Maybe.map .length
                |> Maybe.withDefault Quantity.zero

        updatedParameter =
            updateParameter velocity delta path.parameter

        ( nextSplineIdx, nextSplineMeta, nextParameter ) =
            if Quantity.ratio updatedParameter currentSplineLength >= 0.995 then
                let
                    idxPlusOne =
                        path.currentSplineIdx + 1
                in
                ( idxPlusOne
                , Array.get idxPlusOne path.splines
                  -- if the parameter overflows (more than the spline length), add the remainder to the next spline's parameter
                , updatedParameter
                    |> Quantity.minus currentSplineLength
                    |> Quantity.max Quantity.zero
                )

            else
                ( path.currentSplineIdx
                , path.currentSpline
                , updatedParameter
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


routeTrafficControl : World -> Route -> Maybe ( TrafficControl, Point2d Length.Meters GlobalCoordinates )
routeTrafficControl world route =
    route
        |> Route.splineEndPoint
        |> Maybe.andThen (World.findNodeByPosition world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    trafficControlResult =
                        RoadNetwork.nodeTrafficControl nodeCtx
                in
                ( trafficControlResult, nodeCtx.node.label.position )
            )


restoreRoute : World -> Car -> Result String Route
restoreRoute world car =
    if not <| Route.isReroutable car.route then
        -- If the car is unrouted or arriving to the destination, no need to reroute
        Ok car.route

    else
        let
            nextNodePosition =
                case Car.currentState car of
                    Car.Unparking ->
                        Route.startNode car.route
                            |> Maybe.map RoadNetwork.nodePosition

                    _ ->
                        Route.splineEndPoint car.route

            startNodeValidation =
                nextNodePosition
                    |> Result.fromMaybe "Could not find the start node for the route"
                    |> Result.andThen (validateNodeByPosition world)

            endNodeValidation =
                validateEndNode world car.route
        in
        Result.map2 Tuple.pair startNodeValidation endNodeValidation
            |> Result.andThen
                (\( startNodeCtx, endNodeCtx ) ->
                    Route.reroute car.route world.roadNetwork startNodeCtx endNodeCtx
                )


validateNodeByPosition : World -> Point2d Length.Meters GlobalCoordinates -> Result String RNNodeContext
validateNodeByPosition world position =
    World.findNodeByPosition world position
        |> Result.fromMaybe "Node not found"


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


attemptGenerateRouteFromParkingSpot : Car -> ParkingReservation -> World -> Result String World
attemptGenerateRouteFromParkingSpot car parkingReservation world =
    if World.hasPendingTilemapChange world then
        Result.Err "Pending tilemap change, not safe to generate route"

    else
        let
            lotWithParkingLock =
                world
                    |> World.findLotById parkingReservation.lotId
                    |> Maybe.andThen (Lot.acquireParkingLock car.id)
        in
        case lotWithParkingLock of
            Just lot ->
                RoadNetwork.findLotExitNodeByLotId
                    world.roadNetwork
                    parkingReservation.lotId
                    |> Maybe.andThen
                        (\start ->
                            let
                                ( end, nextSeed ) =
                                    findRandomDestinationNode world car start
                            in
                            end
                                |> Maybe.andThen
                                    (Route.fromParkedAtLot
                                        parkingReservation
                                        world.lots
                                        world.roadNetwork
                                        start
                                    )
                                |> Maybe.map
                                    (\route ->
                                        world
                                            |> World.setCar (Car.routed route car)
                                            |> World.setSeed nextSeed
                                            |> World.updateLot lot
                                    )
                        )
                    |> Result.fromMaybe "Could not generate route"

            Nothing ->
                Result.Err "Could not acquire parcking lock for unparking"


attemptGenerateRouteFromNode : Car -> RNNodeContext -> World -> Result String World
attemptGenerateRouteFromNode car startNodeCtx world =
    if World.hasPendingTilemapChange world then
        Result.Err "Can't generate route while tilemap change is pending"

    else
        -- Room for improvement: this step is not required once nodes have stable IDs
        World.findNodeByPosition world startNodeCtx.node.label.position
            |> Maybe.andThen
                (\start ->
                    let
                        ( end, nextSeed ) =
                            findRandomDestinationNode world car start
                    in
                    end
                        |> Maybe.andThen (Tuple.pair start >> Route.fromStartAndEndNodes world.roadNetwork)
                        |> Maybe.map
                            (\route ->
                                world
                                    |> World.setSeed nextSeed
                                    |> World.setCar (Car.routed route car)
                            )
                )
            |> Result.fromMaybe "Could not generate route"


findRandomDestinationNode : World -> Car -> RNNodeContext -> ( Maybe RNNodeContext, Random.Seed )
findRandomDestinationNode world car startNodeCtx =
    let
        ( chance, seedAfterRandomFloat ) =
            Random.step (Random.float 0 1) world.seed

        lookingForLot =
            (chance > lotToNodeRouteTargetRatio)
                && (Collection.size world.lots >= 2)

        predicate =
            if lookingForLot then
                case car.make.role of
                    None ->
                        randomLotMatchPredicate
                            world
                            car
                            startNodeCtx

                    ServiceVehicle ->
                        case RoadNetwork.nodeLotId startNodeCtx of
                            Just _ ->
                                -- No valid lot to look for
                                randomNodeMatchPredicate startNodeCtx

                            Nothing ->
                                -- Try to return the service vehicle to their home lot
                                homeLotMatchPredicate car

            else
                randomNodeMatchPredicate startNodeCtx
    in
    RoadNetwork.getRandomNode world.roadNetwork seedAfterRandomFloat predicate


randomLotMatchPredicate :
    World
    -> Car
    -> RNNodeContext
    -> RNNode
    -> Bool
randomLotMatchPredicate world car startNodeCtx endNode =
    case endNode.label.kind of
        LotEntry lotEntryId ->
            let
                isDifferentLot =
                    case startNodeCtx.node.label.kind of
                        LotExit lotExitId ->
                            lotExitId /= lotEntryId

                        _ ->
                            True

                parkingPermitted =
                    case World.findLotById lotEntryId world of
                        Just lot ->
                            Lot.parkingPermitted (parkingPermissionPredicate car.homeLotId lotEntryId) lot

                        Nothing ->
                            False
            in
            isDifferentLot && parkingPermitted

        _ ->
            False


homeLotMatchPredicate : Car -> RNNode -> Bool
homeLotMatchPredicate car endNode =
    case car.homeLotId of
        Just homeLotId ->
            case endNode.label.kind of
                LotEntry lotEntryId ->
                    lotEntryId == homeLotId

                _ ->
                    False

        Nothing ->
            False


randomNodeMatchPredicate :
    RNNodeContext
    -> RNNode
    -> Bool
randomNodeMatchPredicate startNodeCtx endNode =
    (endNode.label.kind == LaneConnector)
        && (Point2d.distanceFrom
                startNodeCtx.node.label.position
                endNode.label.position
                |> Quantity.greaterThanOrEqualTo minRouteEndNodeDistance
           )


attemptBeginParking : Car -> Id -> World -> Result String World
attemptBeginParking car lotId world =
    case World.findLotById lotId world of
        Just lot ->
            lot
                |> Lot.prepareParking
                    (parkingPermissionPredicate car.homeLotId lotId)
                    car.id
                |> Maybe.andThen
                    (\( lotWithParkingLock, parkingSpot ) ->
                        let
                            parkingReservation =
                                { lotId = lotId
                                , parkingSpotId = parkingSpot.id
                                }
                        in
                        Route.arriveToParkingSpot parkingReservation world.lots
                            |> Maybe.map
                                (\route ->
                                    world
                                        |> World.setCar (Car.routedWithParking route parkingReservation car)
                                        |> World.updateLot (Lot.reserveParkingSpot car.id parkingSpot.id lotWithParkingLock)
                                )
                    )
                |> Result.fromMaybe "Lot not ready for parking"

        Nothing ->
            -- The lot has been removed, despawn the car
            Result.Ok
                (World.setCar
                    (Car.triggerDespawn car)
                    world
                )


parkingPermissionPredicate : Maybe Id -> Id -> (ParkingSpot -> Bool)
parkingPermissionPredicate homeLotId lotId =
    if Just lotId == homeLotId then
        Lot.parkingSpotEligibleForResident

    else
        Lot.parkingSpotEligibleForAll
