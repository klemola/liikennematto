port module Savegame exposing
    ( SavegameData
    , clearSavegameUrl
    , decode
    , encode
    , onHashChange
    , onHashCleared
    , spawnLotResidents
    , updateSavegameUrl
    )

import Array
import Data.Lots as Lots exposing (NewLot)
import Data.TileSet
import Dict
import Json.Decode as JD
import Json.Encode as JE
import Lib.Collection as Collection
import Lib.OrthogonalDirection exposing (OrthogonalDirection)
import Lib.SeedState as SeedState exposing (SeedState)
import Model.World as World exposing (World)
import Set
import Simulation.Lot as Lot exposing (Lot)
import Simulation.Traffic
import Tilemap.Cell as Cell exposing (CellCoordinates)
import Tilemap.Core as Tilemap exposing (Tilemap)
import Tilemap.Tile as Tile
import Tilemap.TileConfig as TileConfig exposing (LargeTile, TileId)
import Tilemap.TileInventory as TileInventory
import Time


type alias SavegameData =
    { version : Int
    , seed : List Int
    , tilemapDimensions : ( Int, Int )

    -- 1D array of tile IDs (0 if not tile has been set for the cell)
    , tilemap : List Int

    -- World.lotEntries equivalent as [lotTypeId, x, y]
    , lots : List ( Int, Int, Int )
    }


currentSavegameVersion : Int
currentSavegameVersion =
    1


port updateSavegameUrl : JE.Value -> Cmd msg


port clearSavegameUrl : () -> Cmd msg


port onHashChange : (JE.Value -> msg) -> Sub msg


port onHashCleared : (() -> msg) -> Sub msg



--
-- Encoding
--


encode : World -> JE.Value
encode world =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig world.tilemap

        tilemapArray =
            extractTilemapTileIds world.tilemap

        lotsList =
            Collection.values world.lots
                |> List.filterMap (lotToSavegameData world)
    in
    JE.object
        [ ( "v", JE.int currentSavegameVersion )
        , ( "seed", encodeSeedState world.seedState )
        , ( "tmd"
          , JE.list JE.int
                [ tilemapConfig.horizontalCellsAmount
                , tilemapConfig.verticalCellsAmount
                ]
          )
        , ( "tilemap", JE.list JE.int tilemapArray )
        , ( "lots"
          , JE.list
                (\{ lotId, x, y } ->
                    JE.list JE.int [ lotId, x, y ]
                )
                lotsList
          )
        ]


extractTilemapTileIds : Tilemap -> List Int
extractTilemapTileIds tilemap =
    Tilemap.foldTiles
        (\_ tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    properties.id :: acc

                _ ->
                    -- Use 0 for empty cells (saves bytes compared to null)
                    0 :: acc
        )
        []
        tilemap
        |> List.reverse


lotToSavegameData : World -> Lot -> Maybe { lotId : Int, x : Int, y : Int }
lotToSavegameData world lot =
    case ( Lots.findByName lot.name, findLotEntryCell world lot.id ) of
        ( Just newLot, Just ( x, y ) ) ->
            Just { lotId = newLot.id, x = x, y = y }

        _ ->
            Nothing


findLotEntryCell : World -> Collection.Id -> Maybe ( Int, Int )
findLotEntryCell world lotId =
    Dict.toList world.lotEntries
        |> List.filterMap
            (\( cellCoords, ( entryLotId, _ ) ) ->
                if Collection.idMatches entryLotId lotId then
                    Just cellCoords

                else
                    Nothing
            )
        |> List.head


encodeSeedState : SeedState -> JE.Value
encodeSeedState seedState =
    JE.list JE.int
        [ seedState.initialSeed
        , seedState.stepCount
        ]



--
-- Decoding
--


decode : JE.Value -> Result String World
decode jsonValue =
    JD.decodeValue savegameDecoder jsonValue
        |> Result.mapError JD.errorToString
        |> Result.andThen worldFromSavegameData


savegameDecoder : JD.Decoder SavegameData
savegameDecoder =
    JD.map5 SavegameData
        (JD.field "v" JD.int)
        (JD.field "seed" (JD.list JD.int))
        (JD.field "tmd" tilemapDimensionsDecoder)
        (JD.field "tilemap" (JD.list JD.int))
        (JD.field "lots" (JD.list lotDataDecoder))


tilemapDimensionsDecoder : JD.Decoder ( Int, Int )
tilemapDimensionsDecoder =
    JD.list JD.int
        |> JD.andThen
            (\dimensions ->
                case dimensions of
                    [ w, h ] ->
                        JD.succeed ( w, h )

                    _ ->
                        JD.fail "Expected array of 2 integers for tilemap dimensions"
            )


lotDataDecoder : JD.Decoder ( Int, Int, Int )
lotDataDecoder =
    JD.list JD.int
        |> JD.andThen
            (\data ->
                case data of
                    [ lotId, entryCellX, entryCellY ] ->
                        JD.succeed ( lotId, entryCellX, entryCellY )

                    _ ->
                        JD.fail "Expected array of 3 integers for lot data [lotId, entryCellX, entryCellY]"
            )



--
-- World reconstruction
--


coordinatesToString : CellCoordinates -> String
coordinatesToString ( x, y ) =
    "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"


type alias LotMetadata =
    { name : String
    , entryCell : CellCoordinates
    , entryDirection : OrthogonalDirection
    , drivewayCell : Cell.Cell
    , largeTile : LargeTile
    }


computeParentTilesFromLotMetadata : Tilemap.TilemapConfig -> List LotMetadata -> Dict.Dict CellCoordinates ( TileId, Int )
computeParentTilesFromLotMetadata tilemapConfig lotMetadataList =
    List.foldl
        (\lotMetadata acc ->
            addLotCellsToParentMap tilemapConfig lotMetadata.drivewayCell lotMetadata.largeTile acc
        )
        Dict.empty
        lotMetadataList


addLotCellsToParentMap : Tilemap.TilemapConfig -> Cell.Cell -> LargeTile -> Dict.Dict CellCoordinates ( TileId, Int ) -> Dict.Dict CellCoordinates ( TileId, Int )
addLotCellsToParentMap tilemapConfig drivewayCell largeTile dict =
    let
        maybeTopLeftCell =
            Tile.largeTileTopLeftCell tilemapConfig drivewayCell largeTile.anchorIndex largeTile
    in
    case maybeTopLeftCell of
        Just topLeftCell ->
            let
                subgridConstraints =
                    { horizontalCellsAmount = largeTile.width
                    , verticalCellsAmount = largeTile.height
                    }
            in
            List.range 0 (largeTile.width * largeTile.height - 1)
                |> List.filterMap
                    (\subgridIndex ->
                        Cell.fromArray1DIndex subgridConstraints subgridIndex
                            |> Maybe.andThen (Cell.placeIn tilemapConfig topLeftCell)
                            |> Maybe.map
                                (\globalCell ->
                                    ( Cell.coordinates globalCell
                                    , ( largeTile.id, subgridIndex )
                                    )
                                )
                    )
                |> List.foldl
                    (\( cellCoords, value ) acc -> Dict.insert cellCoords value acc)
                    dict

        Nothing ->
            dict


prepareLotMetadata : Tilemap.TilemapConfig -> ( Int, Int, Int ) -> Result String LotMetadata
prepareLotMetadata tilemapConfig ( lotId, x, y ) =
    let
        entryCellCoords =
            ( x, y )
    in
    case ( Cell.fromCoordinates tilemapConfig entryCellCoords, Lots.findById lotId ) of
        ( Nothing, _ ) ->
            Err ("Invalid lot entry cell coordinates: " ++ coordinatesToString entryCellCoords)

        ( _, Nothing ) ->
            Err ("Unknown lot ID: " ++ String.fromInt lotId)

        ( Just entryCell, Just newLot ) ->
            case Cell.nextOrthogonalCell tilemapConfig newLot.entryDirection entryCell of
                Nothing ->
                    Err ("Cannot compute driveway cell for lot: " ++ newLot.name)

                Just drivewayCell ->
                    case findMatchingLargeTile newLot of
                        Nothing ->
                            Err ("No matching LargeTile found for lot: " ++ newLot.name)

                        Just largeTile ->
                            Ok
                                { name = newLot.name
                                , entryCell = entryCellCoords
                                , entryDirection = newLot.entryDirection
                                , drivewayCell = drivewayCell
                                , largeTile = largeTile
                                }


findMatchingLargeTile : NewLot -> Maybe LargeTile
findMatchingLargeTile lot =
    Data.TileSet.lotTiles
        |> List.filterMap
            (\tileConfig ->
                case tileConfig of
                    TileConfig.Large largeTile ->
                        if World.newLotMatchesTile largeTile lot then
                            Just largeTile

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> List.head


worldFromSavegameData : SavegameData -> Result String World
worldFromSavegameData data =
    if data.version /= currentSavegameVersion then
        Err ("Unsupported savegame version: " ++ String.fromInt data.version)

    else
        let
            prepareLotMetadataResult seedState =
                let
                    ( width, height ) =
                        data.tilemapDimensions

                    tilemapConfig =
                        { horizontalCellsAmount = width
                        , verticalCellsAmount = height
                        }
                in
                data.lots
                    |> List.map (prepareLotMetadata tilemapConfig)
                    |> combineResults
                    |> Result.map (\lotMetadataList -> ( seedState, tilemapConfig, lotMetadataList ))

            restoreTilemapResult ( seedState, tilemapConfig, lotMetadataList ) =
                let
                    worldWithSeed =
                        World.empty seedState.currentSeed tilemapConfig
                            |> (\world -> { world | seedState = seedState })
                in
                restoreTilemap data.tilemap lotMetadataList tilemapConfig worldWithSeed
                    |> Result.map (\worldWithTilemap -> ( lotMetadataList, worldWithTilemap ))

            restoreLotsResult ( lotMetadataList, worldWithTilemap ) =
                restoreLots lotMetadataList worldWithTilemap
                    |> Result.map World.updateRoadNetwork
        in
        seedFromString data.seed
            |> Result.andThen prepareLotMetadataResult
            |> Result.andThen restoreTilemapResult
            |> Result.andThen restoreLotsResult


combineResults : List (Result e a) -> Result e (List a)
combineResults results =
    List.foldr
        (\result acc ->
            Result.map2 (::) result acc
        )
        (Ok [])
        results


seedFromString : List Int -> Result String SeedState
seedFromString seedList =
    case seedList of
        [ initialSeed, stepCount ] ->
            Ok (SeedState.fromIntAndSteps initialSeed stepCount)

        _ ->
            Err "Invalid seed format - expected array of 2 integers [initialSeed, stepCount]"


restoreTilemap : List Int -> List LotMetadata -> Tilemap.TilemapConfig -> World -> Result String World
restoreTilemap tileIds lotMetadataList tilemapConfig world =
    let
        expectedLength =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount
    in
    if List.length tileIds /= expectedLength then
        Err
            ("Tilemap size mismatch: expected "
                ++ String.fromInt expectedLength
                ++ " tiles, got "
                ++ String.fromInt (List.length tileIds)
            )

    else
        let
            tilemapArray =
                Array.fromList tileIds

            parentTilesDict =
                computeParentTilesFromLotMetadata tilemapConfig lotMetadataList

            tilemap =
                Tilemap.createTilemap tilemapConfig
                    (\index ->
                        case Array.get index tilemapArray of
                            Just tileId ->
                                if tileId == 0 then
                                    Tile.init Tile.Unintialized

                                else
                                    let
                                        parentTileInfo =
                                            Cell.fromArray1DIndex tilemapConfig index
                                                |> Maybe.andThen (\cell -> Dict.get (Cell.coordinates cell) parentTilesDict)
                                    in
                                    createTileFromId tileId parentTileInfo

                            Nothing ->
                                Tile.init Tile.Unintialized
                    )
        in
        Ok (World.setTilemap tilemap world)


createTileFromId : Int -> Maybe ( Int, Int ) -> Tile.Tile
createTileFromId tileId parentTileInfo =
    let
        tileConfig =
            Data.TileSet.tileById tileId

        ( tile, _ ) =
            Tile.fromTileConfig tileConfig parentTileInfo Tile.AddFromSaveGame
    in
    tile


restoreLots : List LotMetadata -> World -> Result String World
restoreLots lotMetadataList world =
    List.foldl
        (\lotMetadata ->
            Result.andThen (restoreSingleLot lotMetadata)
        )
        (Ok world)
        lotMetadataList
        |> Result.map rebuildTileInventory


rebuildTileInventory : World -> World
rebuildTileInventory world =
    let
        placedLots =
            Collection.values world.lots
                |> List.map .name
                |> Set.fromList

        updatedInventory =
            Dict.map
                (\_ newLots ->
                    List.filter (\lot -> not (Set.member lot.name placedLots)) newLots
                )
                world.tileInventory
    in
    { world | tileInventory = updatedInventory }


restoreSingleLot : LotMetadata -> World -> Result String World
restoreSingleLot lotMetadata world =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig world.tilemap

        maybeEntryCell =
            Cell.fromCoordinates tilemapConfig lotMetadata.entryCell

        maybeNewLot =
            Lots.findByName lotMetadata.name
    in
    case ( maybeEntryCell, maybeNewLot ) of
        ( Just lotEntryCell, Just newLot ) ->
            let
                ( lot, nextLots ) =
                    Collection.addFromBuilder
                        (Lot.build newLot lotEntryCell)
                        world.lots

                nextTilemap =
                    case
                        Tile.largeTileTopLeftCell
                            tilemapConfig
                            lotMetadata.drivewayCell
                            lotMetadata.largeTile.anchorIndex
                            lotMetadata.largeTile
                    of
                        Just topLeftCell ->
                            Tilemap.mapCell topLeftCell (Tile.withName lotMetadata.name) world.tilemap

                        Nothing ->
                            world.tilemap

                nextWorld =
                    { world
                        | lots = nextLots
                        , tilemap = nextTilemap
                    }
                        |> World.addLotEntry lotEntryCell lot.id newLot.entryDirection
            in
            Ok nextWorld

        ( Nothing, _ ) ->
            Err
                ("Invalid lot entry cell coordinates: "
                    ++ coordinatesToString lotMetadata.entryCell
                )

        ( _, Nothing ) ->
            Err ("Unknown lot name: " ++ lotMetadata.name)



--
-- Resident spawning
--


spawnLotResidents : Time.Posix -> World -> World
spawnLotResidents currentTime world =
    Collection.foldl
        (\lotId lot updatedWorld ->
            case Lots.findByName lot.name of
                Just newLot ->
                    Simulation.Traffic.addLotResidents currentTime lotId newLot.residents updatedWorld

                Nothing ->
                    updatedWorld
        )
        world
        world.lots
