module Simulation.Events exposing (processEvents, updateEventQueue)

import Common exposing (addMillisecondsToPosix)
import Dict
import Direction2d
import EventQueue
import Model.Car as Car exposing (Car, CarEvent(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (orthogonalDirectionToLmDirection)
import Model.Lot as Lot exposing (Lot)
import Model.World as World exposing (World, WorldEvent(..))
import Random
import Result.Extra
import Simulation.Traffic exposing (spawnResident, spawnTestCar)
import Time



--
-- Events from simulation
--


processEvents : Time.Posix -> Random.Seed -> List ( Id, CarEvent ) -> World -> World
processEvents time seed events world =
    List.foldl (processEvent time seed)
        world
        events


processEvent : Time.Posix -> Random.Seed -> ( Id, CarEvent ) -> World -> World
processEvent time seed ( carId, event ) world =
    case event of
        ParkingStarted ->
            -- TODO: retry
            attemptReserveParkingSpot carId world

        ParkingComplete ->
            parkingCompleteEffects carId world

        UnparkingStarted ->
            -- Room for improvement: instead of despawn, the parking lock could be retried
            leaveParkingSpot carId world

        UnparkingComplete ->
            leaveLot carId world

        DespawnComplete ->
            setupRespawn time seed carId world


attemptReserveParkingSpot : Id -> World -> World
attemptReserveParkingSpot =
    withCar
        (withParking
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
        (withParking
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
        (withParking
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
        (withParking
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


setupRespawn : Time.Posix -> Random.Seed -> Id -> World -> World
setupRespawn time seed =
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

                ( delay, _ ) =
                    Random.step (Random.int minDelay maxDelay) seed

                triggerAt =
                    addMillisecondsToPosix delay time
            in
            world
                |> World.removeCar car.id
                |> World.addEvent eventKind triggerAt
        )



--
-- Dequeue
--


updateEventQueue : Time.Posix -> Random.Seed -> World -> World
updateEventQueue time seed world =
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
                            spawnTestCar seed nextWorld
                    in
                    -- The car might not have been spawned, but it's not important enough to retry
                    worldWithCar
        )
        { world | eventQueue = nextQueue }
        triggeredEvents



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


withParking : (Id -> Car -> Lot -> World -> World) -> Car -> World -> World
withParking mapFn car world =
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
