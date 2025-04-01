module Simulation.Update exposing (update)

import Data.TileSet exposing (extractLotEntryTile, lotDrivewayTileIds, tileById)
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
import Simulation.Lot as Lot
import Simulation.Traffic as Traffic exposing (addLotResidents, rerouteCarsIfNeeded)
import Simulation.TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( anchorByCell
        , fixedTileByCell
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
            ( { model
                | world = worldAfterTilemapChange tilemapChange worldWithLots
              }
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


worldAfterTilemapChange : TilemapChange -> World -> World
worldAfterTilemapChange tilemapChange world =
    world
        |> removeOrphanLots tilemapChange
        |> updateRoadNetwork
        |> rerouteCarsIfNeeded


removeOrphanLots : TilemapChange -> World -> World
removeOrphanLots tilemapChange world =
    tilemapChange.changedCells
        |> Nonempty.toList
        |> List.filterMap
            (\cell ->
                anchorByCell world.tilemap cell |> Maybe.map (\( id, _ ) -> ( id, cell ))
            )
        |> List.foldl
            (\( anchorLotId, cell ) nextWorld ->
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
                                World.removeLot anchorLotId nextWorld

                    Nothing ->
                        -- The lot entry tile has been removed
                        World.removeLot anchorLotId nextWorld
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
                    ( withLot, lotPlacement ) =
                        addLot time lotTileConfig drivewayCell nextWorld
                in
                ( withLot, lotPlacement :: lotsAdded )
            )
            ( world, [] )


addLot : Time.Posix -> LargeTile -> Cell -> World -> ( World, LotPlacement )
addLot time largeTile drivewayCell ({ tilemap, lots } as world) =
    let
        ( newLot, worldWithUpdatedTileInventory ) =
            World.prepareNewLot largeTile world

        constraints =
            getTilemapConfig tilemap

        anchorCell =
            -- Use of unsafe function: to get here, the next Cell has to exist (it is the lot entry cell)
            Cell.nextOrthogonalCellUnsafe
                constraints
                (OrthogonalDirection.opposite newLot.entryDirection)
                drivewayCell

        builderFn =
            Lot.build newLot anchorCell

        ( lot, nextLots ) =
            Collection.addFromBuilder builderFn lots
    in
    ( worldWithUpdatedTileInventory
        |> World.refreshLots lot nextLots
        |> addLotResidents time lot.id newLot.residents
    , { lot = lot
      , tile = largeTile
      , drivewayCell = drivewayCell
      , anchorCell = anchorCell
      }
    )
