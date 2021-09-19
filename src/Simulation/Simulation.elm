module Simulation.Simulation exposing
    ( Model
    , Msg(..)
    , SimulationState(..)
    , init
    , maxCarSpawnQueueSize
    , subscriptions
    , update
    )

import Browser.Events as Events
import Common
import Config
import Defaults
import Dict
import Dict.Extra as Dict
import Direction2d
import Duration
import Length
import Model.Board as Board
import Model.Car as Car exposing (Car, Status(..))
import Model.Cell as Cell exposing (Cell)
import Model.Entity as Entity
import Model.Geometry exposing (LMEntityCoordinates)
import Model.Lot as Lot exposing (NewLot)
import Model.RoadNetwork exposing (ConnectionKind(..))
import Model.TrafficLight as TrafficLight
import Model.World as World exposing (World)
import Point2d
import Process
import QuadTree exposing (QuadTree)
import Quantity
import Random
import Random.List
import Simulation.Pathfinding as Pathfinding
import Simulation.RoadNetwork exposing (findNodeByLotId, findNodeByNodeId, getOutgoingConnections)
import Simulation.Round as Round
import Simulation.Steering as Steering
import Simulation.WorldUpdate exposing (addLot, setCar, spawnCar)
import Task
import Time


type alias Model =
    { seed : Random.Seed
    , simulation : SimulationState
    , carSpawnQueue : Int
    , carPositionLookup : QuadTree Length.Meters LMEntityCoordinates Car
    }


type SimulationState
    = Running
    | Paused


type Msg
    = SetSimulation SimulationState
    | UpdateTraffic Float
    | UpdateEnvironment Time.Posix
    | GenerateEnvironment ()
    | CheckQueues Time.Posix
    | CheckCarStatus Time.Posix
    | SpawnTestCar
    | VisibilityChanged Events.Visibility


init : ( Model, Cmd Msg )
init =
    let
        seed =
            Random.initialSeed 666
    in
    ( { seed = seed
      , simulation = Running
      , carSpawnQueue = 0
      , carPositionLookup = QuadTree.init Board.boundingBox quadTreeLeafElementsAmount
      }
    , generateEnvironmentAfterDelay seed
    )


environmentUpdateFrequency : Float
environmentUpdateFrequency =
    1000


dequeueFrequency : Float
dequeueFrequency =
    500


carStatusCheckFrequency : Float
carStatusCheckFrequency =
    1000


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


maxCarSpawnQueueSize : Int
maxCarSpawnQueueSize =
    5


subscriptions : Model -> Sub Msg
subscriptions { simulation } =
    if simulation == Paused then
        Events.onVisibilityChange VisibilityChanged

    else
        Sub.batch
            [ Events.onVisibilityChange VisibilityChanged
            , Events.onAnimationFrameDelta UpdateTraffic
            , Time.every environmentUpdateFrequency UpdateEnvironment
            , Time.every dequeueFrequency CheckQueues
            , Time.every carStatusCheckFrequency CheckCarStatus
            ]



--
-- Core update function and effects
--


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
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
            , world
            , Cmd.none
            )

        SetSimulation simulation ->
            ( { model | simulation = simulation }, world, Cmd.none )

        UpdateTraffic rafDelta ->
            let
                cars =
                    Dict.values world.cars

                carPositionLookup =
                    QuadTree.init Board.boundingBox quadTreeLeafElementsAmount |> QuadTree.insertList cars

                ( nextWorld, nextSeed ) =
                    updateTraffic
                        { updateQueue = cars
                        , carPositionLookup = carPositionLookup
                        , seed = model.seed
                        , world = world
                        , delta = rafDelta
                        }
            in
            ( { model
                | seed = nextSeed
                , carPositionLookup = carPositionLookup
              }
            , nextWorld
            , Cmd.none
            )

        UpdateEnvironment _ ->
            ( model
            , updateEnvironment world
            , Cmd.none
            )

        GenerateEnvironment _ ->
            let
                ( nextWorld, nextSeed ) =
                    attemptGenerateEnvironment world model.seed model.simulation
            in
            ( { model | seed = nextSeed }
            , nextWorld
            , generateEnvironmentAfterDelay nextSeed
            )

        CheckQueues _ ->
            let
                ( nextWorld, nextCarSpawnQueue, nextSeed ) =
                    dequeueCarSpawn model.carSpawnQueue model.seed world
            in
            ( { model
                | carSpawnQueue = nextCarSpawnQueue
                , seed = nextSeed
              }
            , nextWorld
            , Cmd.none
            )

        CheckCarStatus _ ->
            let
                ( nextWorld, nextSeed ) =
                    checkCarStatus model.seed world
            in
            ( { model | seed = nextSeed }
            , nextWorld
            , Cmd.none
            )

        SpawnTestCar ->
            ( { model | carSpawnQueue = model.carSpawnQueue + 1 }
            , world
            , Cmd.none
            )


generateEnvironmentAfterDelay : Random.Seed -> Cmd Msg
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
            Dict.size world.board > 4 * max 1 (Dict.size world.lots + 1)

        existingBuildingKinds =
            world.lots
                |> Dict.map (\_ lot -> lot.content.kind)
                |> Dict.values

        unusedLots =
            List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) Defaults.lots
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
        ( shuffledBoard, _ ) =
            Random.step (Random.List.shuffle (Dict.toList world.board)) seed

        targetTile =
            if Cell.isVertical newLot.content.entryDirection then
                Board.twoLaneRoadHorizontal

            else
                Board.twoLaneRoadVertical

        targetDirection =
            Cell.oppositeOrthogonalDirection newLot.content.entryDirection

        isCompatible ( cell, tile ) =
            tile == targetTile && hasEnoughSpaceAround cell && not (World.hasLotAnchor cell world)

        lotBoundingBox cell =
            Lot.bottomLeftCorner ( cell, targetDirection ) newLot
                |> Common.boundingBoxWithDimensions newLot.width newLot.height

        hasEnoughSpaceAround cell =
            World.isEmptyArea (lotBoundingBox cell) world
                && isNotAtTheEndOfARoad cell

        isNotAtTheEndOfARoad cell =
            Cell.allODs
                |> List.all
                    (\od ->
                        case World.tileAt (Cell.next od cell) world of
                            Just neighbor ->
                                neighbor == Board.twoLaneRoadHorizontal || neighbor == Board.twoLaneRoadVertical

                            Nothing ->
                                True
                    )
    in
    shuffledBoard
        |> List.filter isCompatible
        |> List.head
        |> Maybe.map (\( cell, _ ) -> ( newLot, cell ))



--
-- Traffic logic (cars)
--


updateTraffic :
    { updateQueue : List Car
    , carPositionLookup : QuadTree Length.Meters LMEntityCoordinates Car
    , seed : Random.Seed
    , world : World
    , delta : Float
    }
    -> ( World, Random.Seed )
updateTraffic { updateQueue, carPositionLookup, seed, world, delta } =
    case updateQueue of
        [] ->
            ( world, seed )

        activeCar :: queue ->
            let
                otherCars =
                    carPositionLookup
                        |> QuadTree.neighborsWithin Config.tileSizeInMeters activeCar.boundingBox
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
                , carPositionLookup = carPositionLookup
                , world = setCar nextCar.id nextCar world
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
                    getOutgoingConnections nodeCtx
                        |> Random.List.choose
                        |> Random.map Tuple.first

                ( connection, nextSeed ) =
                    Random.step randomConnectionGenerator seed

                nextCar =
                    connection
                        |> Maybe.andThen (findNodeByNodeId world.roadNetwork)
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


checkCarStatus : Random.Seed -> World -> ( World, Random.Seed )
checkCarStatus seed world =
    let
        weightedCoinToss =
            Random.weighted ( 70, True ) [ ( 30, False ) ]

        ( toss, nextSeed ) =
            Random.step weightedCoinToss seed

        statusCheck _ car =
            case car.status of
                Confused ->
                    if Car.isStoppedOrWaiting car then
                        car.homeLotId |> Maybe.map (moveCarToHome world car)

                    else
                        Just car

                ParkedAtLot ->
                    if toss then
                        car.homeLotId
                            |> Maybe.andThen (findNodeByLotId world.roadNetwork)
                            |> Maybe.map
                                (\nodeCtx ->
                                    car
                                        |> Pathfinding.createRoute nodeCtx
                                        |> Steering.startMoving
                                )

                    else
                        Just car

                Moving ->
                    Just car
    in
    ( { world | cars = world.cars |> Dict.filterMap statusCheck }, nextSeed )


moveCarToHome : World -> Car -> Entity.Id -> Car
moveCarToHome world car lotId =
    let
        home =
            Dict.get lotId world.lots

        homeNode =
            findNodeByLotId world.roadNetwork lotId
    in
    case ( home, homeNode ) of
        ( Just lot, Just nodeCtx ) ->
            { car
                | position = lot.entryDetails.parkingSpot
                , orientation = Lot.parkingSpotOrientation lot
                , status = Car.ParkedAtLot
                , velocity = Quantity.zero
                , acceleration = Steering.maxAcceleration
                , route = []
                , localPath = []
            }
                |> Pathfinding.createRoute nodeCtx

        _ ->
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
