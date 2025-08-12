module Simulation.Update exposing (addLot, update)

import Common exposing (addMillisecondsToPosix)
import Data.Lots exposing (NewLot)
import Data.TileSet exposing (extractLotEntryTile, lotDrivewayTileIds, tileById)
import Dict
import Duration
import Lib.Collection as Collection
import Lib.FSM as FSM
import Lib.OrthogonalDirection as OrthogonalDirection
import List.Nonempty as Nonempty
import Message exposing (Message(..))
import Model.Debug exposing (DevAction(..))
import Model.Liikennematto
    exposing
        ( Liikennematto
        )
import Model.World as World
    exposing
        ( LotPlacement
        , TilemapChange
        , World
        , updateRoadNetwork
        )
import Simulation.Events exposing (updateEventQueue)
import Simulation.Lot as Lot exposing (Lot)
import Simulation.Traffic as Traffic exposing (addLotResidents)
import Simulation.TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( fixedTileByCell
        , getTilemapConfig
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (TileKind(..))
import Tilemap.TileConfig as TileConfig exposing (LargeTile)
import Time



--
-- Core update function and effects
--


update : Message -> Liikennematto -> ( Liikennematto, Cmd Message )
update msg model =
    case msg of
        GameSetupComplete ->
            ( model, Cmd.none )

        UpdateTraffic delta ->
            let
                cars =
                    -- TODO: fold to avoid double iteration
                    Collection.values model.world.cars

                ( worldWithTrafficUpdate, worldEvents ) =
                    Traffic.updateTraffic
                        { updateQueue = cars
                        , world = model.world
                        , delta = delta
                        , events = []
                        }
            in
            ( { model
                | world =
                    processWorldEvents
                        model.time
                        worldEvents
                        worldWithTrafficUpdate
              }
            , Cmd.none
            )

        CheckQueues time _ ->
            ( { model
                | world = updateEventQueue time model.world
                , time = time
              }
            , Cmd.none
            )

        TilemapChanged tilemapChange ->
            let
                ( worldWithLots, lotPlacements ) =
                    newLotsFromTilemapChange tilemapChange model.time model.world
            in
            ( { model | world = worldAfterTilemapChange model.time tilemapChange worldWithLots }
            , if List.isEmpty lotPlacements then
                Cmd.none

              else
                Message.asCmd (LotsPlaced lotPlacements)
            )

        UpdateEnvironment ->
            ( { model | world = updateTrafficLights model.world }
            , Cmd.none
            )

        TriggerDevAction action ->
            case action of
                SpawnTestCar ->
                    ( { model
                        | world =
                            World.addEvent
                                World.SpawnTestCar
                                model.time
                                model.world
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )



--
-- World effects
--


processWorldEvents : Time.Posix -> List World.WorldEvent -> World -> World
processWorldEvents time events world =
    List.foldl
        (\event nextWorld ->
            if event == World.None then
                nextWorld

            else
                -- Trigger on next update
                World.addEvent event time nextWorld
        )
        world
        events


worldAfterTilemapChange : Time.Posix -> TilemapChange -> World -> World
worldAfterTilemapChange time tilemapChange world =
    world
        |> removeOrphanLots tilemapChange
        |> updateRoadNetwork
        |> World.addEvent
            World.RerouteCars
            (addMillisecondsToPosix 1 time)


removeOrphanLots : TilemapChange -> World -> World
removeOrphanLots tilemapChange world =
    tilemapChange.changedCells
        |> Nonempty.toList
        |> List.filterMap
            (\cell ->
                world.lotEntries
                    |> Dict.get (Cell.coordinates cell)
                    |> Maybe.map (\( id, _ ) -> ( id, cell ))
            )
        |> List.foldl
            (\( lotEntryLotId, cell ) nextWorld ->
                case fixedTileByCell world.tilemap cell of
                    Just tile ->
                        let
                            lotEntryTile =
                                tile
                                    |> Tile.id
                                    |> Maybe.andThen extractLotEntryTile
                        in
                        case lotEntryTile of
                            Just _ ->
                                nextWorld

                            Nothing ->
                                -- The tile used to be a lot entry tile, yet has changed
                                World.removeLot lotEntryLotId nextWorld

                    Nothing ->
                        -- The lot entry tile has been removed
                        World.removeLot lotEntryLotId nextWorld
            )
            world



--
-- Environment and zoning
--


updateTrafficLights : World -> World
updateTrafficLights world =
    { world
        | trafficLights =
            Collection.map
                (\_ trafficLight -> updateTrafficLight trafficLight)
                world.trafficLights
    }


updateTrafficLight : TrafficLight -> TrafficLight
updateTrafficLight trafficLight =
    let
        -- FSM change actions are ignored
        ( nextFsm, _ ) =
            FSM.updateWithoutContext (Duration.seconds 1) trafficLight.fsm
    in
    { trafficLight | fsm = nextFsm }


newLotsFromTilemapChange : TilemapChange -> Time.Posix -> World -> ( World, List LotPlacement )
newLotsFromTilemapChange tilemapChange time world =
    let
        extractDrivewayTile cell tile =
            case tile.kind of
                Fixed properties ->
                    if List.member properties.id lotDrivewayTileIds then
                        properties.parentTile
                            |> Maybe.andThen
                                (\( parentTileId, _ ) ->
                                    case tileById parentTileId of
                                        TileConfig.Large largeTile ->
                                            Just largeTile

                                        TileConfig.Single _ ->
                                            Nothing
                                )
                            |> Maybe.map (Tuple.pair cell)

                    else
                        Nothing

                _ ->
                    Nothing
    in
    tilemapChange.changedCells
        |> Nonempty.toList
        |> List.filterMap
            (\cell ->
                tileByCell world.tilemap cell
                    |> Maybe.andThen (extractDrivewayTile cell)
            )
        |> List.foldl
            (\( drivewayCell, lotTileConfig ) ( nextWorld, lotsAdded ) ->
                let
                    ( newLot, worldWithUpdatedTileInventory ) =
                        World.prepareNewLot lotTileConfig nextWorld

                    onCreateLot lot worldWithLot =
                        addLotResidents time lot.id newLot.residents worldWithLot

                    ( withLot, lotPlacement ) =
                        addLot onCreateLot lotTileConfig newLot drivewayCell worldWithUpdatedTileInventory
                in
                ( withLot, lotPlacement :: lotsAdded )
            )
            ( world, [] )


addLot : (Lot -> World -> World) -> LargeTile -> NewLot -> Cell -> World -> ( World, LotPlacement )
addLot onCreateLot largeTile newLot drivewayCell ({ tilemap, lots } as world) =
    let
        constraints =
            getTilemapConfig tilemap

        lotEntryCell =
            -- Use of unsafe function: to get here, the next Cell has to exist (it is the lot entry cell)
            Cell.nextOrthogonalCellUnsafe
                constraints
                (OrthogonalDirection.opposite newLot.entryDirection)
                drivewayCell

        builderFn =
            Lot.build newLot lotEntryCell

        ( lot, nextLots ) =
            Collection.addFromBuilder builderFn lots
    in
    ( world
        |> World.refreshLots lot nextLots
        |> onCreateLot lot
        |> World.addLotEntry
            lotEntryCell
            lot.id
            lot.entryDirection
    , { lot = lot
      , tile = largeTile
      , drivewayCell = drivewayCell
      , lotEntryCell = lotEntryCell
      }
    )
