module Simulation exposing
    ( Model
    , Msg(..)
    , SimulationState(..)
    , init
    , maxCarSpawnQueueSize
    , subscriptions
    , update
    )

import Board
import Browser.Events as Events
import Car exposing (Car, Status(..))
import Cell exposing (Cell)
import Config
import Dict
import Direction2d
import Duration
import Geometry exposing (LMEntityCoordinates)
import Length
import Lot exposing (NewLot)
import Point2d
import Process
import QuadTree exposing (QuadTree)
import Quantity
import Random
import Random.List
import RoadNetwork
import Round
import Steering
import Task
import Time
import TrafficLight
import World exposing (World)


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
    | SpawnTestCar


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


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


maxCarSpawnQueueSize : Int
maxCarSpawnQueueSize =
    5


subscriptions : Model -> Sub Msg
subscriptions { simulation } =
    if simulation == Paused then
        Sub.none

    else
        Sub.batch
            [ Time.every Config.environmentUpdateFrequency UpdateEnvironment
            , Time.every Config.dequeueFrequency CheckQueues
            , Events.onAnimationFrameDelta UpdateTraffic
            ]



--
-- Core update function and effects
--


update : World -> Msg -> Model -> ( Model, World, Cmd Msg )
update world msg model =
    case msg of
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
            List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) Lot.all
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
                |> Maybe.map (\lot -> World.addLot lot world)
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
                |> Geometry.boundingBoxWithDimensions newLot.width newLot.height

        hasEnoughSpaceAround cell =
            World.isEmptyArea (lotBoundingBox cell) world
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
                , world = world |> World.setCar nextCar.id nextCar
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
            chooseNextConnection seed world car


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
                                (if nodeKind == RoadNetwork.DeadendExit || nodeKind == RoadNetwork.LaneStart then
                                    { car
                                        | orientation = Direction2d.toAngle nodeCtx.node.label.direction
                                        , position = nodeCtx.node.label.position
                                    }

                                 else
                                    car
                                )
                                    |> Car.createRoute nextNodeCtx
                            )
                        |> Maybe.withDefault car
            in
            ( nextCar, nextSeed )

        _ ->
            ( Car.markAsConfused car, seed )


updateCar : Float -> Car -> Car
updateCar delta car =
    case car.status of
        -- TODO: trigger car movement from "ParkedAtLot" state separately, and possibly randomly
        ParkedAtLot ->
            Car.startMoving car

        _ ->
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
                World.spawnCar seed world
        in
        case newCarId of
            Just _ ->
                ( nextWorld, queue - 1, nextSeed )

            Nothing ->
                ( nextWorld, queue, nextSeed )

    else
        ( world, queue, seed )
