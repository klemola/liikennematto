module Simulation.WorldUpdate exposing (..)

-- TODO: rename module

import BoundingBox2d
import Defaults
import Dict
import Dict.Extra as Dict
import Direction2d
import Model.Board as Board exposing (Board)
import Model.Car as Car exposing (Car, CarKind(..), Cars, Status(..))
import Model.Cell exposing (Cell)
import Model.Entity as Entity exposing (Id)
import Model.Lot as Lot exposing (Lot, Lots)
import Model.RoadNetwork exposing (RNNodeContext, RoadNetwork)
import Model.World exposing (World)
import Random
import Simulation.Pathfinding as Pathfinding
import Simulation.RoadNetwork as RoadNetwork
import Simulation.Steering as Steering



--
-- Modifications
--


addLot : Lot -> World -> World
addLot lot world =
    let
        nextLotId =
            Entity.nextId world.lots

        nextLots =
            Dict.insert nextLotId lot world.lots

        ( roadNetwork, nextTrafficLights ) =
            RoadNetwork.fromBoardAndLots world.board nextLots
                |> RoadNetwork.setupTrafficControl world.trafficLights
    in
    { world
        | lots = nextLots
        , roadNetwork = roadNetwork
        , trafficLights = nextTrafficLights
    }
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
        |> Maybe.andThen Defaults.resident
        |> Maybe.map (createCar >> addToWorld)
        |> Maybe.withDefault world


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


setCar : Id -> Car -> World -> World
setCar id car world =
    { world | cars = Dict.insert id car world.cars }


buildRoadAt : Cell -> World -> World
buildRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.insert cell Board.defaultTile
                |> Board.applyMask
        , world = world
        }


removeRoadAt : Cell -> World -> World
removeRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.remove cell
                |> Board.applyMask
        , world = world
        }



--
-- Utility
--


worldAfterBoardChange : { cell : Cell, nextBoard : Board, world : World } -> World
worldAfterBoardChange { cell, nextBoard, world } =
    let
        nextLots =
            Dict.filter
                (\_ lot ->
                    hasValidAnchorCell nextBoard lot && not (Lot.inBounds cell lot)
                )
                world.lots

        ( nextRoadNetwork, nextTrafficLights ) =
            RoadNetwork.fromBoardAndLots nextBoard nextLots
                |> RoadNetwork.setupTrafficControl world.trafficLights

        nextCars =
            carsAfterBoardChange
                { cell = cell
                , currentCars = world.cars
                , nextLots = nextLots
                , nextRoadNetwork = nextRoadNetwork
                }
    in
    { world
        | board = nextBoard
        , roadNetwork = nextRoadNetwork
        , cars = nextCars
        , lots = nextLots
        , trafficLights = nextTrafficLights
    }


hasValidAnchorCell : Board -> Lot -> Bool
hasValidAnchorCell board lot =
    case Dict.get (Lot.anchorCell lot) board of
        Just tile ->
            tile == Board.twoLaneRoadHorizontal || tile == Board.twoLaneRoadVertical

        Nothing ->
            False


carsAfterBoardChange :
    { cell : Cell
    , currentCars : Cars
    , nextLots : Lots
    , nextRoadNetwork : RoadNetwork
    }
    -> Cars
carsAfterBoardChange { nextLots, currentCars, nextRoadNetwork } =
    let
        connectionLookup =
            RoadNetwork.lookupTree nextRoadNetwork
    in
    currentCars
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        car.kind == TestCar
            )
        |> Dict.map
            (\_ car ->
                case car.status of
                    Car.Moving ->
                        case
                            List.head car.route
                                |> Maybe.andThen (RoadNetwork.findNodeReplacement connectionLookup)
                                |> Maybe.andThen (RoadNetwork.findNodeByNodeId nextRoadNetwork)
                        of
                            Just nodeCtxResult ->
                                Pathfinding.createRoute nodeCtxResult car

                            Nothing ->
                                Steering.markAsConfused car

                    _ ->
                        car
            )


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            car.boundingBox
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            Dict.size world.board > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing
