module Simulation.Pathfinding exposing
    ( attemptBeginParking
    , attemptGenerateRouteFromNode
    , attemptGenerateRouteFromParkingSpot
    , restoreRoute
    , routeTrafficControl
    , updateRoute
    )

import Array
import Collection exposing (Id)
import Common
import Duration exposing (Duration)
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (LMPoint2d)
import Model.Lot as Lot exposing (ParkingReservation, ParkingSpot)
import Model.RoadNetwork as RoadNetwork
    exposing
        ( ConnectionKind(..)
        , RNNode
        , RNNodeContext
        , TrafficControl
        )
import Model.Route as Route exposing (Route)
import Model.World as World exposing (World)
import Point2d
import Quantity
import Random
import Speed exposing (Speed)


minRouteEndNodeDistance : Length
minRouteEndNodeDistance =
    Length.meters 100


updateRoute : Duration -> Car -> ( Route, World.WorldEvent )
updateRoute delta car =
    case car.route of
        Route.Unrouted ->
            ( car.route, World.None )

        Route.Routed routeMeta ->
            let
                nextPath =
                    updatePath car.velocity delta routeMeta.path

                nextRoute =
                    Route.Routed { routeMeta | path = nextPath }
            in
            if nextPath.finished then
                case
                    routeMeta.endNode.node.label.kind
                of
                    LotEntry lotId ->
                        ( Route.Unrouted
                        , if Car.currentState car == Car.Driving then
                            World.BeginCarParking { carId = car.id, lotId = lotId }

                          else
                            World.None
                        )

                    _ ->
                        ( nextRoute, World.CreateRouteFromNode car.id routeMeta.endNode )

            else
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
            startNodeValidation =
                Route.splineEndPoint car.route
                    |> Result.fromMaybe "Spline end point not found"
                    |> Result.andThen (validateNodeByPosition world)

            endNodeValidation =
                validateEndNode world car.route
        in
        Result.map2 Tuple.pair startNodeValidation endNodeValidation
            |> Result.toMaybe
            |> Maybe.andThen
                (\( startNodeCtx, endNodeCtx ) ->
                    Route.reroute car.route world.roadNetwork startNodeCtx endNodeCtx
                )
            |> Maybe.map (\validatedRoute -> { car | route = validatedRoute })
            |> Maybe.withDefaultLazy (\_ -> Car.triggerDespawn car)

    else
        car


validateNodeByPosition : World -> LMPoint2d -> Result String RNNodeContext
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
                parkingReservation.lotId
                    |> RoadNetwork.findLotExitNodeByLotId world.roadNetwork
                    |> Common.andCarry (findRandomDestinationNode world car)
                    |> Maybe.andThen
                        (\( start, end ) ->
                            Route.fromParkedAtLot
                                parkingReservation
                                world.lots
                                world.roadNetwork
                                start
                                end
                        )
                    |> Maybe.map
                        (\route ->
                            world
                                |> World.setCar (Car.routed route car)
                                |> World.updateLot lot
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
            |> Maybe.andThen (findRandomDestinationNode world car)
            |> Maybe.andThen (Route.fromStartAndEndNodes world.roadNetwork startNodeCtx)
            |> Maybe.map
                (\route ->
                    World.setCar
                        (Car.routed route car)
                        world
                )
            |> Result.fromMaybe "Could not generate route"


findRandomDestinationNode : World -> Car -> RNNodeContext -> Maybe RNNodeContext
findRandomDestinationNode world car startNodeCtx =
    let
        ( chance, seedAfterRandomFloat ) =
            Random.step (Random.float 0 1) world.seed

        lookingForLot =
            chance > 0.65 && Collection.size world.lots >= 2

        matchPredicate =
            if lookingForLot then
                randomLotMatchPredicate
                    world
                    car
                    startNodeCtx

            else
                randomNodeMatchPredicate startNodeCtx
    in
    RoadNetwork.getRandomNode world.roadNetwork seedAfterRandomFloat matchPredicate


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
                        Route.arriveToParkingSpot parkingReservation world.lots car.route
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
