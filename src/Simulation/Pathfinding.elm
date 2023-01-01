module Simulation.Pathfinding exposing
    ( carAfterRouteUpdate
    , generateRouteFromNode
    , generateRouteFromParkingSpot
    , restoreRoute
    , routeTrafficControl
    )

import Array
import Dict
import Duration exposing (Duration)
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car)
import Model.Entity exposing (Id)
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


generateRouteFromNode : Random.Seed -> World -> Maybe Id -> RNNodeContext -> Route
generateRouteFromNode seed world homeLotId startNodeCtx =
    findRandomDestinationNode seed world homeLotId startNodeCtx
        -- Seed discarded
        |> Tuple.first
        |> Maybe.map (Tuple.pair startNodeCtx)
        |> Maybe.andThen
            (\( start, end ) ->
                Route.fromStartAndEndNodes world.roadNetwork start end
            )
        |> Maybe.withDefaultLazy
            (\_ ->
                Route.randomFromNode seed 10 world.roadNetwork startNodeCtx
            )


generateRouteFromParkingSpot : Random.Seed -> World -> Maybe Id -> ParkingReservation -> Route
generateRouteFromParkingSpot seed world homeLotId parkingReservation =
    parkingReservation.lotId
        |> RoadNetwork.findLotExitNodeByLotId world.roadNetwork
        |> Maybe.andThen
            (\startNode ->
                findRandomDestinationNode seed world homeLotId startNode
                    -- Seed discarded
                    |> Tuple.first
                    |> Maybe.map (Tuple.pair startNode)
            )
        |> Maybe.andThen
            (\( start, end ) ->
                Route.fromParkedAtLot
                    parkingReservation
                    world.lots
                    world.roadNetwork
                    start
                    end
            )
        -- Will trigger another generate route attempt
        |> Maybe.withDefault Route.initialRoute


findRandomDestinationNode :
    Random.Seed
    -> World
    -> Maybe Id
    -> RNNodeContext
    -> ( Maybe RNNodeContext, Random.Seed )
findRandomDestinationNode seed world homeLotId startNodeCtx =
    let
        ( chance, seedAfterRandomFloat ) =
            Random.step (Random.float 0 1) seed

        lookingForLot =
            chance > 0.65 && Dict.size world.lots >= 2

        matchPredicate =
            if lookingForLot then
                randomLotMatchPredicate
                    world
                    homeLotId
                    startNodeCtx

            else
                randomNodeMatchPredicate startNodeCtx
    in
    RoadNetwork.getRandomNode world.roadNetwork seedAfterRandomFloat matchPredicate


randomLotMatchPredicate :
    World
    -> Maybe Id
    -> RNNodeContext
    -> RNNode
    -> Bool
randomLotMatchPredicate world homeLotId startNodeCtx endNode =
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
                    case Dict.get lotEntryId world.lots of
                        Just lot ->
                            Lot.parkingPermitted (parkingPermissionPredicate homeLotId lotEntryId) lot

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


type RouteUpdateResult
    = BeginRoute
    | ReachEndNode RNNodeContext Route
    | ArrivedAtParkingSpot
    | ArrivedAtNode Route
    | Updated Route


carAfterRouteUpdate : Random.Seed -> World -> Bool -> Duration -> Car -> Car
carAfterRouteUpdate seed world roadNetworkStale delta car =
    case updateRoute delta car of
        Updated nextRoute ->
            { car | route = nextRoute }

        BeginRoute ->
            let
                route =
                    car.parkingReservation
                        |> Maybe.andThen
                            (\parkingReservation ->
                                let
                                    parkingLockSet =
                                        world.lots
                                            |> Dict.get parkingReservation.lotId
                                            |> Maybe.map Lot.hasParkingLockSet
                                            |> Maybe.withDefault False
                                in
                                if parkingLockSet || roadNetworkStale then
                                    Nothing

                                else
                                    Just (generateRouteFromParkingSpot seed world car.homeLotId parkingReservation)
                            )
                        |> Maybe.withDefault Route.initialRoute
            in
            { car | route = route }

        ReachEndNode nodeCtx nextRoute ->
            case
                nodeCtx.node.label.kind
            of
                LotEntry lotId ->
                    world.lots
                        |> Dict.get lotId
                        |> Maybe.andThen (Lot.attemptParking (parkingPermissionPredicate car.homeLotId lotId))
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
                    { car | route = generateRouteFromNode seed world car.homeLotId nodeCtx }

        ArrivedAtParkingSpot ->
            let
                timerGenerator =
                    Random.float 1500 30000 |> Random.map Duration.milliseconds

                ( waitTimer, _ ) =
                    Random.step timerGenerator seed
            in
            { car | route = Route.Unrouted (Just waitTimer) }

        ArrivedAtNode nextRoute ->
            if Car.isDespawning car then
                { car | route = nextRoute }

            else
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

                nextRoute =
                    Route.ArrivingToDestination destination nextPath
            in
            if nextPath.finished then
                case destination of
                    Route.LotParkingSpot ->
                        ArrivedAtParkingSpot

                    Route.RoadNetworkNode ->
                        ArrivedAtNode nextRoute

            else
                Updated nextRoute


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


parkingPermissionPredicate : Maybe Id -> Id -> (ParkingSpot -> Bool)
parkingPermissionPredicate homeLotId lotId =
    if Just lotId == homeLotId then
        Lot.parkingSpotEligibleForResident

    else
        Lot.parkingSpotEligibleForAll
