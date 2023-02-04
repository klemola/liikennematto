module Simulation.Events exposing (updateEventQueue)

import Common exposing (randomFutureTime)
import Dict
import Direction2d
import EventQueue
import Model.Car as Car exposing (Car, CarEvent(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (orthogonalDirectionToLmDirection)
import Model.Lot as Lot exposing (Lot, ParkingReservation)
import Model.RoadNetwork exposing (RNNodeContext)
import Model.World as World exposing (World, WorldEvent(..))
import Random
import Result.Extra
import Simulation.Pathfinding
    exposing
        ( generateRouteFromNode
        , generateRouteFromParkingSpot
        , routeToParkingSpot
        )
import Simulation.Traffic exposing (spawnResident, spawnTestCar)
import Time


updateEventQueue : Time.Posix -> World -> World
updateEventQueue time world =
    let
        ( nextQueue, triggeredEvents ) =
            EventQueue.update time world.eventQueue
    in
    List.foldl
        (processEvent time)
        { world | eventQueue = nextQueue }
        triggeredEvents


processEvent : Time.Posix -> EventQueue.Event WorldEvent -> World -> World
processEvent time event world =
    case event.kind of
        World.SpawnResident carMake lotId ->
            case World.findLotById lotId world of
                Just lot ->
                    withRetry
                        (spawnResident time carMake lot world)
                        time
                        event
                        world

                Nothing ->
                    -- If the lot doesn't exist, then it's ok not to retry
                    world

        World.SpawnTestCar ->
            let
                ( worldWithCar, _ ) =
                    spawnTestCar world
            in
            -- The car might not have been spawned, but it's not important enough to retry
            worldWithCar

        World.CreateRouteFromParkingSpot carId parkingReservation ->
            withCar
                (\car _ ->
                    withRetry
                        (attemptGenerateRouteFromParkingSpot car parkingReservation world)
                        time
                        event
                        world
                )
                carId
                world

        World.CreateRouteFromNode carId startNodeCtx ->
            withCar
                (\car _ ->
                    withRetry
                        (attemptGenerateRouteFromNode car startNodeCtx world)
                        time
                        event
                        world
                )
                carId
                world

        World.BeginCarParking { carId, lotId } ->
            withCar
                (\car _ ->
                    let
                        carWithPendingStateChange =
                            Car.triggerWaitingForParking car

                        updatedWorld =
                            World.setCar carWithPendingStateChange world
                    in
                    withRetry
                        (attemptBeginParking carWithPendingStateChange lotId updatedWorld)
                        time
                        event
                        updatedWorld
                )
                carId
                world

        World.CarStateChange carId carEvent ->
            onCarStateChange time carId carEvent world

        None ->
            world


attemptGenerateRouteFromParkingSpot : Car -> ParkingReservation -> World -> Result String World
attemptGenerateRouteFromParkingSpot car parkingReservation world =
    let
        parkingLockSet =
            world.lots
                |> Dict.get parkingReservation.lotId
                |> Maybe.map Lot.hasParkingLockSet
                |> Maybe.withDefault False
    in
    if parkingLockSet || World.hasPendingTilemapChange world then
        Result.Err "Can't leave the parking spot yet"

    else
        generateRouteFromParkingSpot world car parkingReservation
            |> Result.map (\route -> Car.routed route car)
            |> Result.map (\routedCar -> World.setCar routedCar world)


attemptGenerateRouteFromNode : Car -> RNNodeContext -> World -> Result String World
attemptGenerateRouteFromNode car startNodeCtx world =
    if World.hasPendingTilemapChange world then
        Result.Err "Can't generate route while tilemap change is pending"

    else
        -- Room for improvement: this step is not required once nodes have stable IDs
        World.findNodeByPosition world startNodeCtx.node.label.position
            |> Result.fromMaybe "Could not find the start node"
            |> Result.andThen (generateRouteFromNode world car)
            |> Result.map (\route -> Car.routed route car)
            |> Result.map (\routedCar -> World.setCar routedCar world)


attemptBeginParking : Car -> Id -> World -> Result String World
attemptBeginParking car lotId world =
    case routeToParkingSpot car lotId world of
        Just ( parkingReservation, route ) ->
            let
                nextCar =
                    { car | parkingReservation = Just parkingReservation }
                        |> Car.routed route
            in
            Result.Ok (World.setCar nextCar world)

        Nothing ->
            Result.Err "Could not route car to parking spot"


onCarStateChange : Time.Posix -> Id -> Car.CarEvent -> World -> World
onCarStateChange time carId event world =
    case event of
        ParkingStarted ->
            -- TODO: retry
            attemptReserveParkingSpot carId world

        ParkingComplete ->
            parkingCompleteEffects time carId world

        UnparkingStarted ->
            leaveParkingSpot carId world

        UnparkingComplete ->
            leaveLot carId world

        DespawnComplete ->
            setupRespawn time carId world


attemptReserveParkingSpot : Id -> World -> World
attemptReserveParkingSpot =
    withCar
        (withParkingContext
            (\{ lot, parkingReservation } car world ->
                lot
                    |> Lot.acquireParkingLock car.id
                    |> Maybe.map (Lot.reserveParkingSpot car.id parkingReservation.parkingSpotId)
                    |> Maybe.map (\updatedLot -> World.updateLot updatedLot world)
                    |> Maybe.withDefault world
            )
        )


parkingCompleteEffects : Time.Posix -> Id -> World -> World
parkingCompleteEffects time =
    withCar
        (withParkingContext
            (\{ lot, parkingReservation } car world ->
                let
                    nextOrientation =
                        lot.parkingSpotExitDirection
                            |> orthogonalDirectionToLmDirection
                            |> Direction2d.toAngle

                    nextCar =
                        { car | orientation = nextOrientation }

                    ( triggerAt, nextSeed ) =
                        Random.step
                            (randomFutureTime ( 5000, 45000 ) time)
                            world.seed
                in
                world
                    |> World.setCar nextCar
                    |> World.updateLot (Lot.releaseParkingLock nextCar.id lot)
                    |> World.setSeed nextSeed
                    |> World.addEvent
                        (World.CreateRouteFromParkingSpot car.id parkingReservation)
                        triggerAt
            )
        )


leaveParkingSpot : Id -> World -> World
leaveParkingSpot =
    withCar
        (withParkingContext
            (\{ parkingReservation, lot } car world ->
                case Lot.acquireParkingLock car.id lot of
                    Just lotWithLock ->
                        World.updateLot lotWithLock world

                    Nothing ->
                        -- The parking lock should have been free but was not
                        -- Room for improvement: acquire the parking lock when before unparking
                        world
                            |> World.setCar (Car.triggerDespawn car)
                            |> World.updateLot (Lot.unreserveParkingSpot parkingReservation.parkingSpotId lot)
            )
        )


leaveLot : Id -> World -> World
leaveLot =
    withCar
        (withParkingContext
            (\{ parkingReservation, lot } car world ->
                let
                    nextLot =
                        lot
                            |> Lot.releaseParkingLock car.id
                            |> Lot.unreserveParkingSpot parkingReservation.parkingSpotId
                in
                World.updateLot nextLot world
            )
        )


setupRespawn : Time.Posix -> Id -> World -> World
setupRespawn time =
    withCar
        (\car world ->
            let
                ( eventKind, maxDelay ) =
                    case car.homeLotId of
                        Just homeLotId ->
                            ( SpawnResident car.make homeLotId, 20 * 1000 )

                        Nothing ->
                            ( SpawnTestCar, 3 * 1000 )

                minDelay =
                    maxDelay // 2

                ( triggerAt, nextSeed ) =
                    Random.step
                        (randomFutureTime ( minDelay, maxDelay ) time)
                        world.seed
            in
            world
                |> World.setSeed nextSeed
                |> World.removeCar car.id
                |> World.addEvent eventKind triggerAt
        )



--
-- Utility
--


withRetry : Result String World -> Time.Posix -> EventQueue.Event WorldEvent -> World -> World
withRetry result time event world =
    world.eventQueue
        |> EventQueue.try
            (\_ -> result)
            event
            time
        |> Result.Extra.extract
            (\nextEventQueue ->
                { world | eventQueue = nextEventQueue }
            )


withCar : (Car -> World -> World) -> Id -> World -> World
withCar mapFn carId world =
    case World.findCarById carId world of
        Just car ->
            mapFn car world

        Nothing ->
            world


type alias ParkingContext =
    { lot : Lot
    , parkingReservation : Lot.ParkingReservation
    }


withParkingContext : (ParkingContext -> Car -> World -> World) -> Car -> World -> World
withParkingContext mapFn car world =
    car.parkingReservation
        |> Maybe.andThen
            (\parkingReservation ->
                Dict.get parkingReservation.lotId world.lots
                    |> Maybe.map
                        (\lot ->
                            mapFn
                                { lot = lot
                                , parkingReservation = parkingReservation
                                }
                                car
                                world
                        )
            )
        |> Maybe.withDefault world
