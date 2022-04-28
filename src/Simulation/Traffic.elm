module Simulation.Traffic exposing
    ( addLotResident
    , rerouteCarsIfNeeded
    , spawnCar
    , updateTraffic
    )

import BoundingBox2d
import Common
import Data.Cars exposing (CarMake, testCar)
import Data.Lots
import Dict
import Dict.Extra as Dict
import Direction2d
import Duration exposing (Duration)
import FSM
import Length exposing (Length)
import Maybe.Extra as Maybe
import Model.Car as Car exposing (Car, CarState(..))
import Model.Entity as Entity exposing (Id)
import Model.Lookup exposing (carPositionLookup)
import Model.Lot as Lot exposing (Lot, ParkingSpot)
import Model.RoadNetwork as RoadNetwork exposing (ConnectionKind(..), RNNodeContext)
import Model.World as World exposing (World)
import Point2d
import Polyline2d
import QuadTree
import Quantity
import Random
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

                fsmUpdateContext =
                    { currentPosition = activeCar.position
                    , route = activeCar.route
                    , localPath = activeCar.localPath
                    }

                ( nextFSM, _ ) =
                    FSM.update delta fsmUpdateContext activeCar.fsm

                carWithUpdatedFSM =
                    { activeCar | fsm = nextFSM }

                ( carAfterSteeringAndPathfinding, nextSeed ) =
                    case FSM.toCurrentState nextFSM of
                        Car.Parked ->
                            ( Just carWithUpdatedFSM, seed )

                        Car.ReRouting ->
                            -- Temporary implementation; pathfinding doesn't support search for nearest node
                            ( Just
                                (carWithUpdatedFSM
                                    |> Car.triggerDespawn
                                    |> Steering.stop
                                )
                            , seed
                            )

                        Car.Despawning ->
                            ( carWithUpdatedFSM, seed )
                                |> applyRound world otherCars
                                |> Tuple.mapFirst Just

                        Car.Despawned ->
                            ( carAfterDespawn world carWithUpdatedFSM, seed )

                        _ ->
                            carWithUpdatedFSM
                                |> Pathfinding.updatePath world seed
                                |> applyRound world otherCars
                                |> Tuple.mapFirst Just
            in
            updateTraffic
                { updateQueue = queue
                , seed = nextSeed
                , world =
                    carAfterSteeringAndPathfinding
                        |> Maybe.map (updateCar delta)
                        |> Maybe.map (\updatedCar -> World.setCar updatedCar.id updatedCar world)
                        |> Maybe.withDefaultLazy (\_ -> World.removeCar activeCar.id world)
                , delta = delta
                }


applyRound : World -> List Car -> ( Car, Random.Seed ) -> ( Car, Random.Seed )
applyRound world otherCars ( activeCar, seed ) =
    let
        round =
            { world = world
            , otherCars = otherCars
            , activeCar = activeCar
            , seed = seed
            }

        roundResults =
            Round.play round
    in
    ( roundResults.car, roundResults.seed )


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
                    Common.angleToTarget car.position next
                        |> Maybe.withDefault car.orientation

                [] ->
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
            Car.adjustedShape car.make nextPosition nextOrientation
    in
    { car
        | position = nextPosition
        , orientation = nextOrientation
        , velocity = nextVelocity
        , rotation = nextRotation
        , shape = nextShape
        , boundingBox = nextBoundingBox
    }


carAfterDespawn : World -> Car -> Maybe Car
carAfterDespawn world car =
    car.homeLotId
        |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)
        |> Maybe.andThen (\lot -> Lot.findFreeParkingSpot car.id lot |> Maybe.map (Tuple.pair lot))
        |> Maybe.map (moveCarToHome world car)


moveCarToHome : World -> Car -> ( Lot, ParkingSpot ) -> Car
moveCarToHome world car ( home, parkingSpot ) =
    let
        homeNode =
            car.homeLotId |> Maybe.andThen (RoadNetwork.findLotExitByNodeId world.roadNetwork)

        ( nextFSM, _ ) =
            FSM.reset car.fsm
    in
    case homeNode of
        Just nodeCtx ->
            { car
                | fsm = nextFSM
                , position = parkingSpot.position
                , orientation = Lot.parkingSpotOrientation home
                , velocity = Quantity.zero
                , acceleration = Steering.maxAcceleration
                , route = []
                , localPath = Polyline2d.fromVertices []
            }
                |> Pathfinding.createRouteToLotExit nodeCtx parkingSpot.pathToLotExit

        _ ->
            car


rerouteCarsIfNeeded : World -> World
rerouteCarsIfNeeded world =
    let
        nextCars =
            Dict.map (\_ car -> Pathfinding.restoreRoute world car) world.cars
    in
    { world | cars = nextCars }



--
-- Cars & Lots
--


addLotResident : Id -> Lot -> World -> World
addLotResident lotId lot world =
    let
        carId =
            Entity.nextId world.cars

        -- Room for improvement: create the route and path after a resident is spawned so that
        -- a resident can be added before the road network is updated
        homeNode =
            RoadNetwork.findLotExitByNodeId world.roadNetwork lotId

        carMake =
            Data.Lots.resident lot.kind

        parkingSpot =
            Lot.claimParkingSpot carId lot
    in
    Maybe.map3
        (createResident world carId lot)
        homeNode
        carMake
        parkingSpot
        |> Maybe.withDefault world


createResident : World -> Id -> Lot -> RNNodeContext -> CarMake -> ParkingSpot -> World
createResident world carId lot homeNode make parkingSpot =
    let
        lotWithClaimedParkingSpot =
            Lot.updateParkingSpot parkingSpot lot

        car =
            createCar carId lotWithClaimedParkingSpot make homeNode parkingSpot
    in
    { world
        | cars = Dict.insert carId car world.cars
        , lots = Dict.insert lot.id lotWithClaimedParkingSpot world.lots
    }


createCar : Id -> Lot -> CarMake -> RNNodeContext -> ParkingSpot -> Car
createCar carId lot make homeNode parkingSpot =
    Car.new make
        |> Car.withHome lot.id
        |> Car.withPosition parkingSpot.position
        |> Car.withOrientation (Lot.parkingSpotOrientation lot)
        |> Car.build carId
        |> Pathfinding.createRouteToLotExit homeNode parkingSpot.pathToLotExit
        |> Steering.startMoving



--
-- Spawn cars
--


spawnCar : Random.Seed -> World -> ( World, Random.Seed, Maybe Entity.Id )
spawnCar seed world =
    let
        ( maybeRandomNodeCtx, seedAfterRandomNode ) =
            RoadNetwork.getRandomNode world.roadNetwork seed
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    id =
                        Entity.nextId world.cars

                    ( car, seedAfterRouteInit ) =
                        Car.new testCar
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build id
                            |> Pathfinding.initRoute world seedAfterRandomNode nodeCtx
                            |> Tuple.mapFirst Steering.startMoving
                in
                ( { world | cars = Dict.insert id car world.cars }
                , seedAfterRouteInit
                , Just id
                )
            )
        |> Maybe.withDefault ( world, seedAfterRandomNode, Nothing )


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
