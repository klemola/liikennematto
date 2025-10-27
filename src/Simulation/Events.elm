module Simulation.Events exposing (updateEventQueue)

import Common exposing (randomFutureTime)
import Direction2d
import Lib.Collection exposing (Id)
import Lib.EventQueue as EventQueue
import Lib.OrthogonalDirection as OrthogonalDirection
import Model.World as World exposing (World, WorldEvent(..))
import Random
import Result.Extra
import Simulation.Car as Car exposing (Car, CarEvent(..))
import Simulation.Lot as Lot exposing (Lot)
import Simulation.Pathfinding
    exposing
        ( attemptBeginParking
        , attemptGenerateRouteFromNode
        , attemptGenerateRouteFromParkingSpot
        )
import Simulation.Traffic
    exposing
        ( rerouteCarsIfNeeded
        , spawnResident
        , spawnTestCar
        )
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
                        (spawnResident time carMake lot)
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
                        (attemptGenerateRouteFromParkingSpot car parkingReservation)
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
                        (attemptGenerateRouteFromNode car startNodeCtx)
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
                        (attemptBeginParking carWithPendingStateChange lotId)
                        time
                        event
                        updatedWorld
                )
                carId
                world

        World.CarStateChange carId carEvent ->
            onCarStateChange time carId carEvent world

        World.RerouteCars ->
            rerouteCarsIfNeeded world

        None ->
            world


onCarStateChange : Time.Posix -> Id -> Car.CarEvent -> World -> World
onCarStateChange time carId event world =
    case event of
        ParkingComplete ->
            parkingCompleteEffects time carId world

        UnparkingComplete ->
            leaveLot carId world

        DespawnComplete ->
            setupRespawn time carId world


parkingCompleteEffects : Time.Posix -> Id -> World -> World
parkingCompleteEffects time =
    withCar
        (withParkingContext
            (\{ lot, parkingReservation } car world ->
                let
                    nextOrientation =
                        lot.parkingSpotExitDirection
                            |> OrthogonalDirection.toDirection2d
                            |> Direction2d.toAngle

                    nextCar =
                        { car | orientation = nextOrientation }

                    ( triggerAt, nextWorld ) =
                        World.stepSeed
                            (randomFutureTime ( 5000, 45000 ) time)
                            world
                in
                nextWorld
                    |> World.setCar nextCar
                    |> World.updateLot (Lot.releaseParkingLock nextCar.id lot)
                    |> World.addEvent
                        (World.CreateRouteFromParkingSpot car.id parkingReservation)
                        triggerAt
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

                ( triggerAt, nextWorld ) =
                    World.stepSeed
                        (randomFutureTime ( minDelay, maxDelay ) time)
                        world
            in
            nextWorld
                |> World.removeCar car.id
                |> World.addEvent eventKind triggerAt
        )



--
-- Utility
--


withRetry : (World -> Result String World) -> Time.Posix -> EventQueue.Event WorldEvent -> World -> World
withRetry resultFn time event world =
    world.eventQueue
        |> EventQueue.try
            (\_ -> resultFn world)
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
                World.findLotById parkingReservation.lotId world
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
