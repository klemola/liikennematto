module Simulation.Events exposing (processEvents)

import Dict
import Direction2d
import EventQueue
import Model.Car as Car exposing (Car, CarEvent(..))
import Model.Entity exposing (Id)
import Model.Geometry exposing (orthogonalDirectionToLmDirection)
import Model.Lot as Lot exposing (Lot)
import Model.World as World exposing (World, WorldEvent(..))
import Time


processEvents : Time.Posix -> List ( Id, CarEvent ) -> World -> World
processEvents time events world =
    List.foldl (processEvent time)
        world
        events


processEvent : Time.Posix -> ( Id, CarEvent ) -> World -> World
processEvent time ( carId, event ) world =
    -- let
    --     _ =
    --         Debug.log "process event" ( carId, event )
    -- in
    case event of
        ParkingStarted ->
            -- TODO: retry
            attemptReserveParkingSpot carId world

        ParkingComplete ->
            parkingCompleteEffects carId world

        UnparkingStarted ->
            -- TODO: retry
            leaveParkingSpot carId world

        UnparkingComplete ->
            leaveLot carId world

        DespawnComplete ->
            world

        EnterQueue ->
            delayCarSpawn time carId world


attemptReserveParkingSpot : Id -> World -> World
attemptReserveParkingSpot =
    withCar
        (withParking
            (\parkingSpotId car lot world ->
                lot
                    |> Lot.acquireParkingLock car.id
                    |> Maybe.map (Lot.reserveParkingSpot car.id parkingSpotId)
                    |> Maybe.map (\updatedLot -> World.setLot updatedLot world)
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
                    |> World.setLot (Lot.releaseParkingLock nextCar.id lot)
            )
        )


leaveParkingSpot : Id -> World -> World
leaveParkingSpot =
    withCar
        (withParking
            (\parkingSpotId car lot world ->
                case Lot.acquireParkingLock car.id lot of
                    Just lotWithLock ->
                        World.setLot lotWithLock world

                    Nothing ->
                        -- The parking lock should have been free but was not
                        -- Room for improvement: acquire the parking lock when before unparking
                        world
                            |> World.setCar (Car.triggerDespawn car)
                            |> World.setLot (Lot.unreserveParkingSpot parkingSpotId lot)
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
                World.setLot nextLot world
            )
        )


delayCarSpawn : Time.Posix -> Id -> World -> World
delayCarSpawn time =
    withCar
        (\car world ->
            let
                eventKind =
                    case car.homeLotId of
                        Just homeLotId ->
                            SpawnResident car.make homeLotId

                        Nothing ->
                            SpawnTestCar

                triggerAt =
                    (Time.posixToMillis time + 5000) |> Time.millisToPosix

                queueEvent =
                    EventQueue.createEvent eventKind triggerAt
            in
            { world
                | eventQueue =
                    world.eventQueue |> EventQueue.addEvent queueEvent
            }
        )



--
-- Utility
--


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
