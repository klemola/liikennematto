module Model.World exposing
    ( PendingTilemapChange
    , RNLookupEntry
    , World
    , WorldEvent(..)
    , addEvent
    , connectLotToRoadNetwork
    , createLookup
    , createRoadNetwork
    , empty
    , findCarById
    , findLotById
    , findNearbyEntities
    , findNodeByPosition
    , formatEvents
    , hasLot
    , hasPendingTilemapChange
    , isEmptyArea
    , refreshCars
    , refreshLots
    , removeCar
    , removeLot
    , resolveTilemapUpdate
    , setCar
    , setSeed
    , setTilemap
    , updateLot
    , updateRoadNetwork
    )

import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarMake)
import Duration exposing (Duration)
import Graph
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection, Id)
import Lib.EventQueue as EventQueue exposing (EventQueue)
import Point2d
    exposing
        ( Point2d
        )
import QuadTree exposing (Bounded, QuadTree)
import Quantity
import Random
import Round
import Set exposing (Set)
import Simulation.Car as Car exposing (Car)
import Simulation.Lot as Lot exposing (Lot)
import Simulation.RoadNetwork as RoadNetwork exposing (RNNodeContext, RoadNetwork, buildRoadNetwork)
import Simulation.TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell, CellCoordinates)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , TilemapUpdateResult
        , createTilemap
        , getTilemapConfig
        , inTilemapBounds
        , removeAnchor
        , tilemapBoundingBox
        , tilemapIntersects
        )
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
    , roadNetworkLookup : QuadTree Length.Meters GlobalCoordinates RNLookupEntry
    , trafficLights : Collection TrafficLight
    , lots : Collection Lot
    , cars : Collection Car
    , carLookup : QuadTree Length.Meters GlobalCoordinates Car
    , lotLookup : QuadTree Length.Meters GlobalCoordinates Lot
    , eventQueue : EventQueue WorldEvent
    , seed : Random.Seed
    }


type alias PendingTilemapChange =
    ( Duration, Set CellCoordinates )


type alias RNLookupEntry =
    { id : Int
    , position : Point2d Length.Meters GlobalCoordinates
    , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
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


empty : TilemapConfig -> World
empty tilemapConfig =
    let
        tilemap =
            createTilemap tilemapConfig

        worldBB =
            tilemapBoundingBox tilemap
    in
    { tilemap = tilemap
    , pendingTilemapChange = Nothing
    , roadNetwork = RoadNetwork.empty
    , trafficLights = Collection.empty
    , cars = Collection.empty
    , lots = Collection.empty
    , carLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , lotLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , roadNetworkLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , eventQueue = EventQueue.empty
    , seed = initialSeed
    }



--
-- Queries
--


boundingBox : World -> BoundingBox2d Length.Meters GlobalCoordinates
boundingBox world =
    tilemapBoundingBox world.tilemap


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Collection.values lots)


isEmptyArea : BoundingBox2d Length.Meters GlobalCoordinates -> World -> Bool
isEmptyArea testAreaBB world =
    let
        tilemapOverlap =
            tilemapIntersects testAreaBB world.tilemap

        lotOverlap =
            Collection.foldl (\_ lot acc -> lot.boundingBox :: acc) [] world.lots
                |> List.any (Common.boundingBoxOverlaps testAreaBB)
    in
    inTilemapBounds world.tilemap testAreaBB && not lotOverlap && not tilemapOverlap


findCarById : Id -> World -> Maybe Car
findCarById id world =
    Collection.get id world.cars


findLotById : Id -> World -> Maybe Lot
findLotById id world =
    Collection.get id world.lots


findLotByCarPosition : Car -> World -> Maybe Lot
findLotByCarPosition car world =
    world.lotLookup
        |> findNearbyEntitiesFromPoint (Length.meters 0.1) car.position
        |> List.head


findNodeByPosition : World -> Point2d Length.Meters GlobalCoordinates -> Maybe RNNodeContext
findNodeByPosition { roadNetworkLookup, roadNetwork } nodePosition =
    roadNetworkLookup
        |> QuadTree.neighborsWithin
            (Length.meters 1)
            (BoundingBox2d.singleton nodePosition)
        |> List.head
        |> Maybe.andThen (.id >> RoadNetwork.nodeById roadNetwork)


findNearbyEntities :
    Length
    -> BoundingBox2d Length.Meters GlobalCoordinates
    -> QuadTree Length.Meters GlobalCoordinates (Bounded Length.Meters GlobalCoordinates a)
    -> List (Bounded Length.Meters GlobalCoordinates a)
findNearbyEntities radius bb quadTree =
    QuadTree.neighborsWithin radius bb quadTree


findNearbyEntitiesFromPoint :
    Length
    -> Point2d Length.Meters GlobalCoordinates
    -> QuadTree Length.Meters GlobalCoordinates (Bounded Length.Meters GlobalCoordinates a)
    -> List (Bounded Length.Meters GlobalCoordinates a)
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


refreshCars : Collection Car -> World -> World
refreshCars nextCars world =
    { world | cars = nextCars }


setCar : Car -> World -> World
setCar car world =
    { world | cars = Collection.update car.id car world.cars }


removeCar : Id -> World -> World
removeCar carId world =
    case Collection.get carId world.cars of
        Just car ->
            let
                baseWorld =
                    findLotByCarPosition car world
                        |> Maybe.map (lotAfterCarDespawn car)
                        |> Maybe.map (\lot -> updateLot lot world)
                        |> Maybe.withDefault world
            in
            { baseWorld | cars = Collection.remove carId baseWorld.cars }

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


refreshLots : Lot -> Collection Lot -> World -> World
refreshLots lot nextLots world =
    { world
        | lots = nextLots
        , lotLookup = QuadTree.insert lot world.lotLookup
    }


updateLot : Lot -> World -> World
updateLot lot world =
    { world | lots = Collection.update lot.id lot world.lots }


removeLot : Id -> World -> World
removeLot lotId world =
    case Collection.get lotId world.lots of
        Just lot ->
            let
                nextCars =
                    Collection.map
                        (\_ car ->
                            let
                                isInsideLot =
                                    lot.boundingBox |> BoundingBox2d.contains car.position

                                homeRemoved =
                                    case car.homeLotId of
                                        Just homeLotId ->
                                            homeLotId == lotId

                                        Nothing ->
                                            False
                            in
                            if isInsideLot || homeRemoved then
                                Car.triggerDespawn car

                            else
                                car
                        )
                        world.cars

                nextLots =
                    Collection.remove lotId world.lots

                nextTilemap =
                    removeAnchor lotId world.tilemap

                nextLookup =
                    createLookup (Collection.values nextLots) world
            in
            { world
                | lots = nextLots
                , tilemap = nextTilemap
                , cars = nextCars
                , lotLookup = nextLookup
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


createRoadNetwork : Tilemap -> World -> World
createRoadNetwork tilemap world =
    let
        nextWorld =
            { world | tilemap = tilemap }
    in
    updateRoadNetwork nextWorld


connectLotToRoadNetwork : World -> World
connectLotToRoadNetwork =
    -- Room for improvement: re-building the whole roadnetwork when a new lot is added is not optimal
    updateRoadNetwork


updateRoadNetwork : World -> World
updateRoadNetwork world =
    -- Room for improvement: the road network should be updated with minimal changes instead of being replaced
    let
        ( nextRoadNetwork, nextTrafficLights ) =
            buildRoadNetwork world.tilemap world.trafficLights

        nodeLookupList =
            Graph.fold
                (\nodeCtx acc ->
                    { id = nodeCtx.node.id
                    , position = nodeCtx.node.label.position
                    , boundingBox = BoundingBox2d.singleton nodeCtx.node.label.position
                    }
                        :: acc
                )
                []
                nextRoadNetwork
    in
    { world
        | roadNetwork = nextRoadNetwork
        , trafficLights = nextTrafficLights
        , roadNetworkLookup = createLookup nodeLookupList world
    }



--
-- Tilemap change
--


resolveTilemapUpdate : Duration -> TilemapUpdateResult -> World -> ( World, List Cell )
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
                , Cell.fromCoordinatesSet (getTilemapConfig world.tilemap) nextChangedCells
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
            "Spawn resident: lot #" ++ Collection.idToString lotId

        CreateRouteFromParkingSpot carId _ ->
            "Route car from parkingSpot: #" ++ Collection.idToString carId

        CreateRouteFromNode carId _ ->
            "Route car from node: #" ++ Collection.idToString carId

        BeginCarParking { carId, lotId } ->
            "Begin parking: car #" ++ Collection.idToString carId ++ "\nLot #" ++ Collection.idToString lotId

        CarStateChange carId _ ->
            "Car FSM event: #" ++ Collection.idToString carId

        None ->
            "_NONE_"
