module Model.World exposing
    ( PendingTilemapChange
    , RNLookupEntry
    , TilemapChange
    , World
    , WorldEvent(..)
    , addEvent
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
    , prepareNewLot
    , refreshCars
    , refreshLots
    , removeCar
    , removeLot
    , resolveTilemapUpdate
    , setCar
    , setSeed
    , setTilemap
    , tileInventoryCount
    , updateLot
    , updateRoadNetwork
    )

import BoundingBox2d exposing (BoundingBox2d)
import Common exposing (GlobalCoordinates)
import Data.Cars exposing (CarMake)
import Data.Lots exposing (NewLot, allLots)
import Data.TileSet exposing (lotTiles)
import Dict
import Duration exposing (Duration)
import Graph
import Length exposing (Length)
import Lib.Collection as Collection exposing (Collection, Id)
import Lib.EventQueue as EventQueue exposing (EventQueue)
import List.Nonempty exposing (Nonempty)
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
        , removeAnchor
        , tilemapBoundingBox
        )
import Tilemap.Tile as Tile
import Tilemap.TileConfig as TileConfig exposing (TileConfig)
import Tilemap.TileInventory as TileInventory exposing (TileInventory)
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
    , tileInventory : TileInventory (List NewLot)
    , pendingTilemapChange : Maybe PendingTilemapChange
    , roadNetwork : RoadNetwork
    , trafficLights : Collection TrafficLight
    , lots : Collection Lot
    , cars : Collection Car
    , roadNetworkLookup : QuadTree Length.Meters GlobalCoordinates RNLookupEntry
    , carLookup : QuadTree Length.Meters GlobalCoordinates Car
    , lotLookup : QuadTree Length.Meters GlobalCoordinates Lot
    , eventQueue : EventQueue WorldEvent
    , seed : Random.Seed
    }


type alias PendingTilemapChange =
    ( Duration, Set CellCoordinates )


type alias TilemapChange =
    { changedCells : Nonempty Cell
    , transitionedCells : List Cell
    }


type alias RNLookupEntry =
    { id : Int
    , position : Point2d Length.Meters GlobalCoordinates
    , boundingBox : BoundingBox2d Length.Meters GlobalCoordinates
    }


initialTileInventory : TileInventory (List NewLot)
initialTileInventory =
    let
        matchesTile largeTile newLot =
            newLot.horizontalTilesAmount == largeTile.width && newLot.verticalTilesAmount == largeTile.height
    in
    lotTiles
        |> List.filterMap
            (\tileConfig ->
                case tileConfig of
                    TileConfig.Large largeTile ->
                        Just
                            ( largeTile.id
                            , List.filter (matchesTile largeTile) allLots
                            )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


quadTreeLeafElementsAmount : Int
quadTreeLeafElementsAmount =
    4


minTilemapChangeFrequency : Duration
minTilemapChangeFrequency =
    Duration.milliseconds 750


empty : Random.Seed -> TilemapConfig -> World
empty initialSeed tilemapConfig =
    let
        tilemap =
            createTilemap tilemapConfig (\_ -> Tile.init Tile.Unintialized)

        worldBB =
            tilemapBoundingBox tilemap
    in
    { tilemap = tilemap
    , tileInventory = initialTileInventory
    , pendingTilemapChange = Nothing
    , roadNetwork = RoadNetwork.empty
    , trafficLights = Collection.empty
    , cars = Collection.empty
    , lots = Collection.empty
    , roadNetworkLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , carLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
    , lotLookup = QuadTree.init worldBB quadTreeLeafElementsAmount
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


prepareNewLot : TileConfig -> World -> ( NewLot, World )
prepareNewLot tileConfig world =
    let
        tileId =
            TileConfig.tileConfigId tileConfig

        ( ( newLot, remainingOptions ), nextSeed ) =
            TileInventory.chooseRandom tileId world.seed world.tileInventory
    in
    ( newLot |> Maybe.withDefault (newLotFallback tileConfig)
    , { world
        | seed = nextSeed
        , tileInventory = Dict.insert tileId remainingOptions world.tileInventory
      }
    )


tileInventoryCount : World -> TileInventory Int
tileInventoryCount world =
    TileInventory.countAvailable world.tileInventory


newLotFallback : TileConfig -> NewLot
newLotFallback tileConfig =
    case TileConfig.tileConfigId tileConfig of
        100 ->
            Data.Lots.residentialSingle1

        101 ->
            Data.Lots.school

        102 ->
            Data.Lots.fireStation

        103 ->
            Data.Lots.residentialRow1

        104 ->
            Data.Lots.residentialApartments1

        _ ->
            Data.Lots.residentialSingle1


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

                tileInventoryWithRestoredLot =
                    TileInventory.restoreItem
                        (\( _, newLot ) -> newLot.name == lot.name)
                        (TileInventory.tileIdItemPairs initialTileInventory)
                        world.tileInventory
            in
            { world
                | lots = nextLots
                , cars = nextCars
                , tilemap = removeAnchor lotId world.tilemap
                , lotLookup = createLookup (Collection.values nextLots) world
                , tileInventory = tileInventoryWithRestoredLot
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
    updateRoadNetwork (setTilemap tilemap world)


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


resolveTilemapUpdate : Duration -> TilemapUpdateResult -> World -> ( World, Maybe TilemapChange )
resolveTilemapUpdate delta tilemapUpdateResult world =
    case world.pendingTilemapChange of
        Nothing ->
            let
                nextWorld =
                    if List.isEmpty tilemapUpdateResult.transitionedCells then
                        world

                    else
                        createPendingTilemapChange tilemapUpdateResult world
            in
            ( nextWorld
            , Nothing
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
                    combineChangedCells tilemapUpdateResult currentChangedCells
            in
            if Quantity.lessThanOrEqualToZero nextTimer then
                ( { world | pendingTilemapChange = Nothing }
                , nextChangedCells
                    |> Cell.fromCoordinatesSet (getTilemapConfig world.tilemap)
                    |> List.Nonempty.fromList
                    |> Maybe.map
                        (\changedCells ->
                            { changedCells = changedCells
                            , transitionedCells = tilemapUpdateResult.transitionedCells
                            }
                        )
                )

            else
                ( { world | pendingTilemapChange = Just ( nextTimer, nextChangedCells ) }
                , Nothing
                )


createPendingTilemapChange : TilemapUpdateResult -> World -> World
createPendingTilemapChange tilemapUpdateResult world =
    let
        pendingTilemapChange =
            Just
                ( minTilemapChangeFrequency
                , combineChangedCells tilemapUpdateResult Set.empty
                )
    in
    { world | pendingTilemapChange = pendingTilemapChange }


combineChangedCells : TilemapUpdateResult -> Set CellCoordinates -> Set CellCoordinates
combineChangedCells tilemapUpdateResult currentChanges =
    let
        transitioned =
            Set.fromList (List.map Cell.coordinates tilemapUpdateResult.transitionedCells)

        emptied =
            Set.fromList (List.map Cell.coordinates tilemapUpdateResult.emptiedCells)
    in
    Set.union (Set.union transitioned emptied) currentChanges



--
-- Utility
--


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
