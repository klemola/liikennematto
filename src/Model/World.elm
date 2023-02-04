module Model.World exposing
    ( PendingTilemapChange
    , RNLookupEntry
    , World
    , WorldEvent(..)
    , addEvent
    , addLot
    , createLookup
    , empty
    , findCarById
    , findLotById
    , findNearbyEntities
    , findNodeByPosition
    , formatEvents
    , hasLot
    , hasPendingTilemapChange
    , isEmptyArea
    , removeCar
    , removeLot
    , resolveTilemapUpdate
    , setCar
    , setSeed
    , setTilemap
    , updateLot
    )

import BoundingBox2d
import Common
import Data.Cars exposing (CarMake)
import Dict exposing (Dict)
import Duration exposing (Duration)
import EventQueue exposing (EventQueue)
import Length exposing (Length)
import Model.Car as Car exposing (Car)
import Model.Cell as Cell exposing (Cell, CellCoordinates)
import Model.Entity exposing (Id)
import Model.Geometry exposing (GlobalCoordinates, LMBoundingBox2d, LMPoint2d)
import Model.Lot as Lot exposing (Lot)
import Model.RoadNetwork as RoadNetwork exposing (RNNodeContext, RoadNetwork)
import Model.Tilemap as Tilemap exposing (Tilemap)
import Model.TrafficLight exposing (TrafficLights)
import QuadTree exposing (Bounded, QuadTree)
import Quantity
import Random
import Round
import Set exposing (Set)
import Time


type WorldEvent
    = SpawnTestCar
    | SpawnResident CarMake Id
    | CreateRouteFromParkingSpot Id Lot.ParkingReservation
    | CreateRouteFromNode Id RNNodeContext
    | BeginCarParking { carId : Id, lotId : Id }
    | CarStateChange Id Car.CarEvent
    | None


type alias World =
    { tilemap : Tilemap
    , pendingTilemapChange : Maybe PendingTilemapChange
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Dict Id Car
    , lots : Dict Id Lot
    , carLookup : QuadTree Length.Meters GlobalCoordinates Car
    , lotLookup : QuadTree Length.Meters GlobalCoordinates Lot
    , roadNetworkLookup : QuadTree Length.Meters GlobalCoordinates RNLookupEntry
    , eventQueue : EventQueue WorldEvent
    , seed : Random.Seed
    }


type alias PendingTilemapChange =
    ( Duration, Set CellCoordinates )


type alias RNLookupEntry =
    { id : Int
    , position : LMPoint2d
    , boundingBox : LMBoundingBox2d
    }


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


minTilemapChangeFrequency : Duration
minTilemapChangeFrequency =
    Duration.milliseconds 750


initialSeed : Random.Seed
initialSeed =
    Random.initialSeed 42


empty : Tilemap.TilemapConfig -> World
empty tilemapConfig =
    let
        tilemap =
            Tilemap.empty tilemapConfig

        worldBB =
            Tilemap.boundingBox tilemap
    in
    { tilemap = tilemap
    , pendingTilemapChange = Nothing
    , roadNetwork = RoadNetwork.empty
    , trafficLights = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    , carLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , lotLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , roadNetworkLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , eventQueue = EventQueue.empty
    , seed = initialSeed
    }



--
-- Queries
--


boundingBox : World -> LMBoundingBox2d
boundingBox world =
    Tilemap.boundingBox world.tilemap


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        tilemapOverlap =
            Tilemap.intersects testAreaBB world.tilemap

        lotOverlap =
            Dict.foldl (\_ lot acc -> lot.boundingBox :: acc) [] world.lots
                |> List.any (Common.boundingBoxOverlaps testAreaBB)
    in
    Tilemap.inBounds world.tilemap testAreaBB && not lotOverlap && not tilemapOverlap


findCarById : Id -> World -> Maybe Car
findCarById id world =
    Dict.get id world.cars


findLotById : Id -> World -> Maybe Lot
findLotById id world =
    Dict.get id world.lots


findLotByCarPosition : Car -> World -> Maybe Lot
findLotByCarPosition car world =
    world.lotLookup
        |> findNearbyEntitiesFromPoint (Length.meters 0.1) car.position
        |> List.head


findNodeByPosition : World -> LMPoint2d -> Maybe RNNodeContext
findNodeByPosition { roadNetworkLookup, roadNetwork } nodePosition =
    roadNetworkLookup
        |> QuadTree.neighborsWithin
            (Length.meters 1)
            (BoundingBox2d.singleton nodePosition)
        |> List.head
        |> Maybe.andThen (.id >> RoadNetwork.findNodeByNodeId roadNetwork)


findNearbyEntities : Length -> LMBoundingBox2d -> QuadTree Length.Meters GlobalCoordinates (Bounded Length.Meters GlobalCoordinates a) -> List (Bounded Length.Meters GlobalCoordinates a)
findNearbyEntities radius bb quadTree =
    QuadTree.neighborsWithin radius bb quadTree


findNearbyEntitiesFromPoint : Length -> LMPoint2d -> QuadTree Length.Meters GlobalCoordinates (Bounded Length.Meters GlobalCoordinates a) -> List (Bounded Length.Meters GlobalCoordinates a)
findNearbyEntitiesFromPoint radius point quadTree =
    findNearbyEntities
        radius
        (BoundingBox2d.singleton point)
        quadTree


hasPendingTilemapChange : World -> Bool
hasPendingTilemapChange editor =
    editor.pendingTilemapChange /= Nothing



--
-- Modification
--


addEvent : WorldEvent -> Time.Posix -> World -> World
addEvent event triggerAt world =
    let
        queueEvent =
            EventQueue.createEvent event triggerAt
    in
    { world | eventQueue = world.eventQueue |> EventQueue.addEvent queueEvent }


setCar : Car -> World -> World
setCar car world =
    { world | cars = Dict.insert car.id car world.cars }


removeCar : Id -> World -> World
removeCar carId world =
    case Dict.get carId world.cars of
        Just car ->
            let
                baseWorld =
                    findLotByCarPosition car world
                        |> Maybe.map (lotAfterCarDespawn car)
                        |> Maybe.map (\lot -> updateLot lot world)
                        |> Maybe.withDefault world
            in
            { baseWorld | cars = Dict.remove carId baseWorld.cars }

        Nothing ->
            world


lotAfterCarDespawn : Car -> Lot -> Lot
lotAfterCarDespawn car lot =
    car.parkingReservation
        |> Maybe.map
            (\{ parkingSpotId } ->
                Lot.unreserveParkingSpot parkingSpotId lot
            )
        |> Maybe.withDefault lot
        |> Lot.releaseParkingLock car.id


addLot : Lot -> World -> World
addLot lot world =
    { world
        | lots = Dict.insert lot.id lot world.lots
        , lotLookup = QuadTree.insert lot world.lotLookup
    }


updateLot : Lot -> World -> World
updateLot lot world =
    { world | lots = Dict.insert lot.id lot world.lots }


removeLot : Id -> World -> World
removeLot lotId world =
    case Dict.get lotId world.lots of
        Just lot ->
            let
                parkedCarsIds =
                    lot.parkingSpots
                        |> List.filterMap (\parkingSpot -> parkingSpot.reservedBy)
                        |> Set.fromList

                nextCars =
                    Dict.map
                        (\_ car ->
                            let
                                isParkedOnLot =
                                    Set.member car.id parkedCarsIds

                                homeRemoved =
                                    case car.homeLotId of
                                        Just homeLotId ->
                                            homeLotId == lotId

                                        Nothing ->
                                            False
                            in
                            if isParkedOnLot || homeRemoved then
                                Car.triggerDespawn car

                            else
                                car
                        )
                        world.cars
            in
            { world
                | lots = Dict.remove lotId world.lots
                , tilemap = Tilemap.removeAnchor lotId world.tilemap
                , cars = nextCars
                , lotLookup = QuadTree.remove lot world.lotLookup
            }

        Nothing ->
            world


setTilemap : Tilemap -> World -> World
setTilemap tilemap world =
    { world | tilemap = tilemap }


createLookup : List (Bounded Length.Meters GlobalCoordinates a) -> World -> QuadTree Length.Meters GlobalCoordinates (Bounded Length.Meters GlobalCoordinates a)
createLookup lookupItems world =
    QuadTree.init (boundingBox world) quadTreeLeafElementsAmount
        |> QuadTree.insertList lookupItems


setSeed : Random.Seed -> World -> World
setSeed seed world =
    { world | seed = seed }



--
-- Tilemap change
--


resolveTilemapUpdate : Duration -> Tilemap.TilemapUpdateResult -> World -> ( World, List Cell )
resolveTilemapUpdate delta tilemapUpdateResult world =
    case world.pendingTilemapChange of
        Nothing ->
            let
                nextWorld =
                    if List.isEmpty tilemapUpdateResult.transitionedCells then
                        world

                    else
                        createPendingTilemapChange tilemapUpdateResult.transitionedCells world
            in
            ( nextWorld
            , []
            )

        Just pendingTilemapChange ->
            let
                ( changeTimer, currentChangedCells ) =
                    pendingTilemapChange

                nextTimer =
                    if not (List.isEmpty tilemapUpdateResult.transitionedCells) then
                        -- The tilemap changed during the delay, reset it (AKA debounce)
                        minTilemapChangeFrequency

                    else
                        changeTimer
                            |> Quantity.minus delta
                            |> Quantity.max Quantity.zero

                nextChangedCells =
                    combineChangedCells tilemapUpdateResult.transitionedCells currentChangedCells
            in
            if Quantity.lessThanOrEqualToZero nextTimer then
                ( { world | pendingTilemapChange = Nothing }
                , Cell.fromCoordinatesSet (Tilemap.config world.tilemap) nextChangedCells
                )

            else
                ( { world | pendingTilemapChange = Just ( nextTimer, nextChangedCells ) }
                , []
                )


createPendingTilemapChange : List Cell -> World -> World
createPendingTilemapChange changedCells editor =
    let
        pendingTilemapChange =
            Just
                ( minTilemapChangeFrequency
                , combineChangedCells changedCells Set.empty
                )
    in
    { editor | pendingTilemapChange = pendingTilemapChange }


combineChangedCells : List Cell -> Set CellCoordinates -> Set CellCoordinates
combineChangedCells changedCells currentChanges =
    changedCells
        |> List.map Cell.coordinates
        |> Set.fromList
        |> Set.union currentChanges



-- Utility


formatEvents : Time.Posix -> World -> List ( String, String, String )
formatEvents time world =
    EventQueue.toList world.eventQueue
        |> List.map
            (\event ->
                let
                    kind =
                        formatEventKind event.kind

                    timeDiff =
                        Time.posixToMillis event.triggerAt - Time.posixToMillis time

                    timeUntilTrigger =
                        Round.round 2 (toFloat timeDiff / 1000) ++ "s"

                    retries =
                        "Retries: " ++ String.fromInt event.retryAmount
                in
                ( kind, timeUntilTrigger, retries )
            )


formatEventKind : WorldEvent -> String
formatEventKind kind =
    case kind of
        SpawnTestCar ->
            "Spawn test car"

        SpawnResident _ lotId ->
            "Spawn resident: lot #" ++ String.fromInt lotId

        CreateRouteFromParkingSpot carId _ ->
            "Route car from parkingSpot: #" ++ String.fromInt carId

        CreateRouteFromNode carId _ ->
            "Route car from node: #" ++ String.fromInt carId

        BeginCarParking { carId, lotId } ->
            "Begin parking: car #" ++ String.fromInt carId ++ "\nLot #" ++ String.fromInt lotId

        CarStateChange carId _ ->
            "Car FSM event: #" ++ String.fromInt carId

        None ->
            "_NONE_"
