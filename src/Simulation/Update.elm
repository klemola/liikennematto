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
        ( TilemapChange
        , World
        , updateRoadNetwork
        )
import Random
import Simulation.Events exposing (updateEventQueue)
import Simulation.Lot as Lot
import Simulation.Traffic as Traffic exposing (addLotResidents, rerouteCarsIfNeeded)
import Simulation.TrafficLight exposing (TrafficLight)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core exposing (addAnchor, anchorByCell, fixedTileByCell, getTilemapConfig, tileByCell)
import Tilemap.Tile exposing (TileKind(..))
import Tilemap.TileConfig exposing (TileConfig)
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
                | world =
                    model.world
                        |> World.setSeed (Random.initialSeed (Time.posixToMillis time))
                        |> updateEventQueue time
                , time = time
              }
            , Cmd.none
            )

        TilemapChanged tilemapChange ->
            ( { model
                | world =
                    model.world
                        |> newLotsFromTilemapChange tilemapChange model.time
                        |> worldAfterTilemapChange tilemapChange
              }
            , Cmd.none
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
                                    |> Tilemap.Tile.id
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


newLotsFromTilemapChange : TilemapChange -> Time.Posix -> World -> World
newLotsFromTilemapChange tilemapChange time world =
    let
        extractDrivewayTile cell tile =
            case tile.kind of
                Fixed properties ->
                    if List.member properties.id lotDrivewayTileIds then
                        properties.parentTile
                            |> Maybe.map (\( parentTileId, _ ) -> tileById parentTileId)
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
            (\( drivewayCell, lotTileConfig ) nextWorld ->
                addLot time lotTileConfig drivewayCell nextWorld
            )
            world


addLot : Time.Posix -> TileConfig -> Cell -> World -> World
addLot time tileConfig drivewayCell ({ tilemap, lots } as world) =
    let
        ( newLot, worldWithUpdatedTileInventory ) =
            World.prepareNewLot tileConfig world

        anchorCell =
            -- Use of unsafe function: to get here, the next Cell has to exist (it is the lot entry cell)
            Cell.nextOrthogonalCellUnsafe
                (getTilemapConfig tilemap)
                newLot.drivewayExitDirection
                drivewayCell

        builderFn =
            Lot.build newLot anchorCell

        ( lot, nextLots ) =
            Collection.addFromBuilder builderFn lots
    in
    worldWithUpdatedTileInventory
        |> World.refreshLots lot nextLots
        |> World.setTilemap
            (addAnchor anchorCell
                lot.id
                (OrthogonalDirection.opposite lot.drivewayExitDirection)
                world.tilemap
            )
        |> addLotResidents time lot.id newLot.residents
