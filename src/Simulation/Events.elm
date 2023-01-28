module Simulation.Events exposing (processEvents, updateEventQueue)

import Common exposing (addMillisecondsToPosix)
import Dict
import Direction2d
import EventQueue
import Model.Car as Car exposing (Car, CarEvent(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (orthogonalDirectionToLmDirection)
import Model.Lot as Lot exposing (Lot, ParkingReservation)
import Model.World as World exposing (World, WorldEvent(..))
import Random
import Result.Extra
import Simulation.Pathfinding exposing (generateRouteFromParkingSpot)
import Simulation.Traffic exposing (spawnResident, spawnTestCar)
import Time



--
-- Events from simulation
--


processEvents : Time.Posix -> List ( Id, CarEvent ) -> World -> World
processEvents time events world =
    List.foldl (processEvent time) world events


processEvent : Time.Posix -> ( Id, CarEvent ) -> World -> World
processEvent time ( carId, event ) world =
    case event of
        ParkingStarted ->
            -- TODO: retry
            attemptReserveParkingSpot carId world

        ParkingComplete ->
            parkingCompleteEffects carId world

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
            (\parkingSpotId car lot world ->
                lot
                    |> Lot.acquireParkingLock car.id
                    |> Maybe.map (Lot.reserveParkingSpot car.id parkingSpotId)
                    |> Maybe.map (\updatedLot -> World.updateLot updatedLot world)
                    |> Maybe.withDefault world
            )
        )


parkingCompleteEffects : Id -> World -> World
parkingCompleteEffects =
    withCar
        (withParkingContext
            (\_ car lot world ->
                let
                    nextOrientation =
                        lot.parkingSpotExitDirection
                            |> orthogonalDirectionToLmDirection
                            |> Direction2d.toAngle

                    nextCar =
                        { car | orientation = nextOrientation }
                in
                world
                    |> World.setCar nextCar
                    |> World.updateLot (Lot.releaseParkingLock nextCar.id lot)
            )
        )


leaveParkingSpot : Id -> World -> World
leaveParkingSpot =
    withCar
        (withParkingContext
            (\parkingSpotId car lot world ->
                case Lot.acquireParkingLock car.id lot of
                    Just lotWithLock ->
                        World.updateLot lotWithLock world

                    Nothing ->
                        -- The parking lock should have been free but was not
                        -- Room for improvement: acquire the parking lock when before unparking
                        world
                            |> World.setCar (Car.triggerDespawn car)
                            |> World.updateLot (Lot.unreserveParkingSpot parkingSpotId lot)
            )
        )


leaveLot : Id -> World -> World
leaveLot =
    withCar
        (withParkingContext
            (\parkingSpotId car lot world ->
                let
                    nextLot =
                        lot
                            |> Lot.releaseParkingLock car.id
                            |> Lot.unreserveParkingSpot parkingSpotId
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

                ( delay, nextSeed ) =
                    Random.step (Random.int minDelay maxDelay) world.seed

                triggerAt =
                    addMillisecondsToPosix delay time
            in
            world
                |> World.setSeed nextSeed
                |> World.removeCar car.id
                |> World.addEvent eventKind triggerAt
        )



--
-- Dequeue
--


updateEventQueue : Time.Posix -> World -> World
updateEventQueue time world =
    let
        ( nextQueue, triggeredEvents ) =
            EventQueue.update time world.eventQueue
    in
    List.foldl
        (\event nextWorld ->
            case event.kind of
                World.SpawnResident carMake lotId ->
                    case World.findLotById lotId nextWorld of
                        Just lot ->
                            withRetry
                                (spawnResident carMake lot nextWorld)
                                time
                                event
                                nextWorld

                        Nothing ->
                            -- If the lot doesn't exist, then it's ok not to retry
                            nextWorld

                World.SpawnTestCar ->
                    let
                        ( worldWithCar, _ ) =
                            spawnTestCar nextWorld
                    in
                    -- The car might not have been spawned, but it's not important enough to retry
                    worldWithCar

                World.RouteCarFromParkingSpot carId parkingReservation ->
                    withCar
                        (\car _ ->
                            withRetry
                                (attemptGenerateRouteFromParkingSpot car parkingReservation nextWorld)
                                time
                                event
                                nextWorld
                        )
                        carId
                        nextWorld
        )
        { world | eventQueue = nextQueue }
        triggeredEvents


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


withParkingContext : (Id -> Car -> Lot -> World -> World) -> Car -> World -> World
withParkingContext mapFn car world =
    case
        car.parkingReservation
            |> Maybe.andThen
                (\{ lotId, parkingSpotId } ->
                    Dict.get lotId world.lots
                        |> Maybe.map (Tuple.pair parkingSpotId)
                )
    of
        Just ( parkingSpotId, lot ) ->
            mapFn parkingSpotId car lot world

        Nothing ->
            world
