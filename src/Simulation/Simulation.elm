module Simulation.Simulation exposing
    ( initCmd
    , spawnCar
    , update
    )

import BoundingBox2d
import Browser.Events as Events
import Common
import Dict
import Dict.Extra as Dict
import Direction2d
import Duration
import Length
import Message exposing (Message(..))
import Model.Car as Car exposing (Car, Cars, Status(..))
import Model.Entity as Entity
import Model.Liikennematto exposing (Liikennematto, SimulationState(..), Tool(..))
import Model.Lookup exposing (carPositionLookup)
import Model.Lot as Lot exposing (Lot, NewLot, allLots)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.Tilemap as Tilemap exposing (Cell, tileSize)
import Model.TrafficLight as TrafficLight
import Model.World as World exposing (World)
import Point2d
import Process
import QuadTree
import Quantity
import Random
import Random.List
import Simulation.Infrastructure as Infrastructure
import Simulation.Pathfinding as Pathfinding
import Simulation.Round as Round
import Simulation.Steering as Steering
import Task


initCmd : Random.Seed -> Cmd Message
initCmd seed =
    generateEnvironmentAfterDelay seed



--
-- Core update function and effects
--


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        VisibilityChanged newVisibility ->
            ( { model
                | simulation =
                    case newVisibility of
                        Events.Visible ->
                            model.simulation

                        Events.Hidden ->
                            Paused
              }
            , Cmd.none
            )

        SetSimulation simulation ->
            ( { model | simulation = simulation }, Cmd.none )

        AddTile cell ->
            let
                worldWithTilemapChange =
                    Infrastructure.buildRoadAt cell model.world

                nextWorld =
                    { worldWithTilemapChange | cars = rerouteCarsIfNeeded worldWithTilemapChange }
            in
            ( { model | world = nextWorld }
            , Cmd.none
            )

        RemoveTile cell ->
            let
                worldWithTilemapChange =
                    Infrastructure.removeRoadAt cell model.world

                nextWorld =
                    { worldWithTilemapChange | cars = rerouteCarsIfNeeded worldWithTilemapChange }
            in
            ( { model | world = nextWorld }
            , Cmd.none
            )

        ResetWorld ->
            ( { model
                | tool = SmartConstruction
                , world = World.empty
              }
            , Cmd.none
            )

        UpdateTraffic rafDelta ->
            let
                cars =
                    Dict.values model.world.cars

                ( nextWorld, nextSeed ) =
                    updateTraffic
                        { updateQueue = cars
                        , seed = model.seed
                        , world = model.world
                        , delta = rafDelta
                        }
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , Cmd.none
            )

        UpdateEnvironment _ ->
            ( { model | world = updateEnvironment model.world }
            , Cmd.none
            )

        GenerateEnvironment _ ->
            let
                ( nextWorld, nextSeed ) =
                    attemptGenerateEnvironment model.world model.seed model.simulation
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , generateEnvironmentAfterDelay nextSeed
            )

        CheckQueues _ ->
            let
                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn model.carSpawnQueue model.seed model.world
            in
            ( { model
                | carSpawnQueue = nextCarSpawnQueue
                , world = nextWorld
                , seed = nextSeed
              }
            , Cmd.none
            )

        CheckCarStatus _ ->
            let
                ( nextWorld, nextSeed ) =
                    checkCarStatus model.seed model.world
            in
            ( { model
                | seed = nextSeed
                , world = nextWorld
              }
            , Cmd.none
            )

        SpawnTestCar ->
            ( { model | carSpawnQueue = model.carSpawnQueue + 1 }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


generateEnvironmentAfterDelay : Random.Seed -> Cmd Message
generateEnvironmentAfterDelay seed =
    let
        randomMillis =
            seed
                |> Random.step (Random.int 1000 3500)
                |> Tuple.first
    in
    randomMillis
        |> toFloat
        |> Process.sleep
        |> Task.perform GenerateEnvironment



--
-- Environment logic
--


updateEnvironment : World -> World
updateEnvironment world =
    let
        nextTrafficLights =
            world.trafficLights
                |> Dict.map (\_ trafficLight -> TrafficLight.advance trafficLight)
    in
    { world | trafficLights = nextTrafficLights }


attemptGenerateEnvironment : World -> Random.Seed -> SimulationState -> ( World, Random.Seed )
attemptGenerateEnvironment world seed simulation =
    let
        largeEnoughRoadNetwork =
            Dict.size world.tilemap > 4 * max 1 (Dict.size world.lots + 1)

        existingBuildingKinds =
            world.lots
                |> Dict.map (\_ lot -> lot.content.kind)
                |> Dict.values

        unusedLots =
            List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) allLots
    in
    if simulation == Paused || List.isEmpty unusedLots || not largeEnoughRoadNetwork then
        ( world, seed )

    else
        generateEnvironment world seed unusedLots


generateEnvironment : World -> Random.Seed -> List NewLot -> ( World, Random.Seed )
generateEnvironment world seed unusedLots =
    let
        randomLot =
            unusedLots
                |> Random.List.choose
                |> Random.map Tuple.first

        ( potentialNewLot, nextSeed ) =
            Random.step randomLot seed

        nextWorld =
            potentialNewLot
                |> Maybe.andThen (findLotAnchor world seed)
                |> Maybe.map Lot.fromNewLot
                |> Maybe.map (\lot -> addLot lot world)
                |> Maybe.withDefault world
    in
    ( nextWorld, nextSeed )


findLotAnchor : World -> Random.Seed -> NewLot -> Maybe ( NewLot, Cell )
findLotAnchor world seed newLot =
    let
        ( shuffledTilemap, _ ) =
            Random.step (Random.List.shuffle (Dict.toList world.tilemap)) seed

        targetTile =
            if Tilemap.isVerticalDirection newLot.content.entryDirection then
                Tilemap.horizontalRoad

            else
                Tilemap.verticalRoad

        targetDirection =
            Tilemap.oppositeOrthogonalDirection newLot.content.entryDirection

        isCompatible ( cell, tile ) =
            tile == targetTile && hasEnoughSpaceAround cell && not (World.hasLotAnchor cell world)

        lotBoundingBox cell =
            Lot.bottomLeftCorner ( cell, targetDirection ) newLot
                |> Common.boundingBoxWithDimensions newLot.width newLot.height

        hasEnoughSpaceAround cell =
            World.isEmptyArea (lotBoundingBox cell) world
                && isNotAtTheEndOfARoad cell

        isNotAtTheEndOfARoad cell =
            Tilemap.orthogonalDirections
                |> List.all
                    (\od ->
                        case Tilemap.tileAt (Tilemap.nextOrthogonalCell od cell) world.tilemap of
                            Just neighbor ->
                                neighbor == Tilemap.horizontalRoad || neighbor == Tilemap.verticalRoad

                            Nothing ->
                                True
                    )
    in
    shuffledTilemap
        |> List.filter isCompatible
        |> List.head
        |> Maybe.map (\( cell, _ ) -> ( newLot, cell ))


addLot : Lot -> World -> World
addLot lot world =
    let
        nextLotId =
            Entity.nextId world.lots

        nextLots =
            Dict.insert nextLotId lot world.lots

        worldWithLots =
            { world | lots = nextLots }
    in
    worldWithLots
        |> Infrastructure.connectLotToRoadNetwork
        |> addLotResident nextLotId lot


addLotResident : Int -> Lot -> World -> World
addLotResident lotId lot world =
    let
        carId =
            Entity.nextId world.cars

        createCar kind =
            Car.new kind
                |> Car.withHome lotId
                |> Car.withPosition lot.entryDetails.parkingSpot
                |> Car.withOrientation (Lot.parkingSpotOrientation lot)
                |> Car.build carId

        addToWorld car =
            { world | cars = Dict.insert carId car world.cars }
    in
    world.lots
        |> Dict.get lotId
        |> Maybe.andThen Lot.resident
        |> Maybe.map (createCar >> addToWorld)
        |> Maybe.withDefault world



--
-- Traffic logic (cars)
--


updateTraffic :
    { updateQueue : List Car
    , seed : Random.Seed
    , world : World
    , delta : Float
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
                        |> QuadTree.neighborsWithin tileSize activeCar.boundingBox
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
    case car.localPath of
        next :: others ->
            if Point2d.equalWithin (Length.meters 0.5) car.position next then
                ( { car | localPath = others }, seed )

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


updateCar : Float -> Car -> Car
updateCar delta car =
    let
        deltaDuration =
            Duration.milliseconds delta

        steering =
            -- TODO: implement proper path following steering to use for movement
            Steering.noSteering

        nextPosition =
            car.position
                |> Point2d.translateIn
                    (Direction2d.fromAngle car.orientation)
                    (car.velocity |> Quantity.for deltaDuration)

        nextOrientation =
            case car.localPath of
                next :: _ ->
                    Steering.angleToTarget car.position next
                        |> Maybe.withDefault car.orientation

                _ ->
                    car.orientation

        nextVelocity =
            car.velocity
                |> Quantity.plus (car.acceleration |> Quantity.for deltaDuration)
                |> Quantity.clamp Quantity.zero Steering.maxVelocity

        nextRotation =
            case steering.angular of
                Just angularAcceleration ->
                    car.rotation |> Quantity.plus (angularAcceleration |> Quantity.for deltaDuration)

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
                , localPath = []
            }
                |> Pathfinding.createRoute nodeCtx

        _ ->
            Steering.markAsConfused car


rerouteCarsIfNeeded : World -> Cars
rerouteCarsIfNeeded world =
    Dict.map (\_ car -> updateRoute world car) world.cars


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
-- Queues
--


dequeueCarSpawn : Int -> Random.Seed -> World -> ( World, Int, Random.Seed )
dequeueCarSpawn queue seed world =
    let
        canSpawnCar =
            queue > 0
    in
    if canSpawnCar then
        let
            ( nextWorld, nextSeed, newCarId ) =
                spawnCar seed world
        in
        case newCarId of
            Just _ ->
                ( nextWorld, queue - 1, nextSeed )

            Nothing ->
                ( nextWorld, queue, nextSeed )

    else
        ( world, queue, seed )


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
            Dict.size world.tilemap > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing
