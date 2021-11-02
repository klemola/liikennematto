module Simulation.Traffic exposing
    ( checkCarStatus
    , rerouteCarsIfNeeded
    , spawnCar
    , updateTraffic
    )

import BoundingBox2d
import Dict
import Dict.Extra as Dict
import Direction2d
import Duration exposing (Duration)
import Length exposing (Length)
import Model.Car as Car exposing (Car, Status(..))
import Model.Entity as Entity
import Model.Lookup exposing (carPositionLookup)
import Model.Lot as Lot exposing (Lot)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.World as World exposing (World)
import Point2d
import Polyline2d
import QuadTree
import Quantity
import Random
import Random.List
import Simulation.Infrastructure as Infrastructure
import Simulation.Pathfinding as Pathfinding
import Simulation.Round as Round
import Simulation.Steering as Steering


nearbyTrafficRadius : Length
nearbyTrafficRadius =
    Length.meters 16


updateTraffic :
    { updateQueue : List Car
    , seed : Random.Seed
    , world : World
    , delta : Duration
    }
    -> ( World, Random.Seed )
updateTraffic { updateQueue, seed, world, delta } =
    case updateQueue of
        [] ->
            ( { world | carPositionLookup = carPositionLookup world.cars }, seed )

        activeCar :: queue ->
            let
                otherCars =
                    world.carPositionLookup
                        |> QuadTree.neighborsWithin nearbyTrafficRadius activeCar.boundingBox
                        |> List.filter (\car -> car.id /= activeCar.id)

                -- 1. Path checks (route, local path)
                ( carAfterPathCheck, seedAfterPathCheck ) =
                    checkPath world seed activeCar

                -- 2. "Round" checks (collision, traffic control)
                round =
                    { world = world
                    , otherCars = otherCars
                    , activeCar = carAfterPathCheck
                    , seed = seedAfterPathCheck
                    }

                roundResults =
                    Round.play round

                -- 3. Car update (velocity, position, bounding box...)
                nextCar =
                    updateCar delta roundResults.car
            in
            updateTraffic
                { updateQueue = queue
                , seed = roundResults.seed
                , world = World.setCar nextCar.id nextCar world
                , delta = delta
                }


checkPath : World -> Random.Seed -> Car -> ( Car, Random.Seed )
checkPath world seed car =
    case Polyline2d.vertices car.localPath of
        next :: others ->
            if Point2d.equalWithin (Length.meters 0.5) car.position next then
                ( { car | localPath = Polyline2d.fromVertices others }, seed )

            else
                ( car, seed )

        [] ->
            case car.status of
                Moving ->
                    chooseNextConnection seed world car

                _ ->
                    ( car, seed )


chooseNextConnection : Random.Seed -> World -> Car -> ( Car, Random.Seed )
chooseNextConnection seed world car =
    case car.route of
        nodeCtx :: _ ->
            let
                randomConnectionGenerator =
                    RoadNetwork.getOutgoingConnections nodeCtx
                        |> Random.List.choose
                        |> Random.map Tuple.first

                ( connection, nextSeed ) =
                    Random.step randomConnectionGenerator seed

                nextCar =
                    connection
                        |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
                        |> Maybe.map
                            (\nextNodeCtx ->
                                -- This is a temporary hack to make sure that tight turns can be completed
                                let
                                    nodeKind =
                                        nodeCtx.node.label.kind
                                in
                                (if nodeKind == DeadendExit || nodeKind == LaneStart then
                                    { car
                                        | orientation = Direction2d.toAngle nodeCtx.node.label.direction
                                        , position = nodeCtx.node.label.position
                                    }

                                 else
                                    car
                                )
                                    |> Pathfinding.createRoute nextNodeCtx
                            )
                        |> Maybe.withDefault car
            in
            ( nextCar, nextSeed )

        _ ->
            ( Steering.markAsConfused car, seed )


updateCar : Duration -> Car -> Car
updateCar delta car =
    let
        steering =
            -- TODO: implement proper path following steering to use for movement
            Steering.noSteering

        nextPosition =
            car.position
                |> Point2d.translateIn
                    (Direction2d.fromAngle car.orientation)
                    (car.velocity |> Quantity.for delta)

        nextOrientation =
            case Polyline2d.vertices car.localPath of
                next :: _ ->
                    Steering.angleToTarget car.position next
                        |> Maybe.withDefault car.orientation

                _ ->
                    car.orientation

        nextVelocity =
            car.velocity
                |> Quantity.plus (car.acceleration |> Quantity.for delta)
                |> Quantity.clamp Quantity.zero Steering.maxVelocity

        nextRotation =
            case steering.angular of
                Just angularAcceleration ->
                    car.rotation |> Quantity.plus (angularAcceleration |> Quantity.for delta)

                Nothing ->
                    Quantity.zero

        ( nextShape, nextBoundingBox ) =
            Car.adjustedShape nextPosition nextOrientation
    in
    { car
        | position = nextPosition
        , orientation = nextOrientation
        , velocity = nextVelocity
        , rotation = nextRotation
        , shape = nextShape
        , boundingBox = nextBoundingBox
    }


weightedCoinToss : Random.Generator Bool
weightedCoinToss =
    Random.weighted ( 70, True ) [ ( 30, False ) ]


checkCarStatus : Random.Seed -> World -> ( World, Random.Seed )
checkCarStatus seed world =
    let
        -- Some status effects are triggered randomly (uniform for all affected cars)
        ( toss, nextSeed ) =
            Random.step weightedCoinToss seed

        nextCars =
            Dict.filterMap (\_ car -> statusCheck world toss car) world.cars

        nextCarPositionLookup =
            carPositionLookup nextCars
    in
    ( { world
        | cars = nextCars
        , carPositionLookup = nextCarPositionLookup
      }
    , nextSeed
    )


statusCheck : World -> Bool -> Car -> Maybe Car
statusCheck world randomCoinToss car =
    -- Room for improvement: the Maybe Lot / Maybe EntityId combo is awkward
    let
        home =
            car.homeLotId |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)
    in
    case car.status of
        Confused ->
            if Car.isStoppedOrWaiting car then
                -- if the car has no home it will be removed
                home |> Maybe.map (moveCarToHome world car)

            else
                Just car

        ParkedAtLot ->
            if randomCoinToss then
                car.homeLotId
                    |> Maybe.andThen (RoadNetwork.findNodeByLotId world.roadNetwork)
                    |> Maybe.map
                        (\nodeCtx ->
                            car
                                |> Pathfinding.createRoute nodeCtx
                                |> Steering.startMoving
                        )

            else
                Just car

        Moving ->
            case home of
                Just _ ->
                    Just car

                Nothing ->
                    if car.kind == Car.TestCar then
                        Just car

                    else
                        Just (Steering.markAsConfused car)


moveCarToHome : World -> Car -> Lot -> Car
moveCarToHome world car home =
    let
        homeNode =
            car.homeLotId |> Maybe.andThen (RoadNetwork.findNodeByLotId world.roadNetwork)
    in
    case homeNode of
        Just nodeCtx ->
            { car
                | position = home.entryDetails.parkingSpot
                , orientation = Lot.parkingSpotOrientation home
                , status = Car.ParkedAtLot
                , velocity = Quantity.zero
                , acceleration = Steering.maxAcceleration
                , route = []
                , localPath = Polyline2d.fromVertices []
            }
                |> Pathfinding.createRoute nodeCtx

        _ ->
            Steering.markAsConfused car



--
-- Routing
--


rerouteCarsIfNeeded : World -> World
rerouteCarsIfNeeded world =
    let
        nextCars =
            Dict.map (\_ car -> updateRoute world car) world.cars
    in
    { world | cars = nextCars }


updateRoute : World -> Car -> Car
updateRoute world car =
    case
        List.head car.route
            |> Maybe.andThen (Infrastructure.findNodeReplacement world)
            |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)
    of
        Just nodeCtxResult ->
            Pathfinding.createRoute nodeCtxResult car

        Nothing ->
            Steering.markAsConfused car



--
-- Spawn cars
--


spawnCar : Random.Seed -> World -> ( World, Random.Seed, Maybe Entity.Id )
spawnCar seed world =
    let
        ( maybeRandomNodeCtx, nextSeed ) =
            RoadNetwork.getRandomNode world.roadNetwork seed
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    id =
                        Entity.nextId world.cars

                    nextNode =
                        RoadNetwork.getOutgoingConnections nodeCtx
                            |> List.head
                            |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)

                    car =
                        Car.new Car.TestCar
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build id
                            |> Pathfinding.maybeCreateRoute nextNode
                            |> Steering.startMoving
                in
                ( { world | cars = Dict.insert id car world.cars }
                , nextSeed
                , Just id
                )
            )
        |> Maybe.withDefault ( world, nextSeed, Nothing )


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            car.boundingBox
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            RoadNetwork.size world.roadNetwork > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing
