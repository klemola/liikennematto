module Savegame exposing
    ( SavegameData
    , decode
    , encode
    , spawnLotResidents
    )

import Array
import Data.Lots as Lots exposing (NewLot)
import Data.TileSet
import Dict
import Json.Decode as JD
import Json.Encode as JE
import Lib.Collection as Collection
import Lib.OrthogonalDirection exposing (OrthogonalDirection)
import Model.World as World exposing (World)
import Random
import Simulation.Lot as Lot exposing (Lot)
import Simulation.Traffic
import Tilemap.Cell as Cell exposing (CellCoordinates)
import Tilemap.Core as Tilemap exposing (Tilemap)
import Tilemap.Tile as Tile
import Tilemap.TileConfig as TileConfig exposing (LargeTile, TileId)
import Time


type alias SavegameData =
    { version : String
    , seed : String
    , tilemapSize : { width : Int, height : Int }

    -- 1D array of tile IDs (null if not tile has been set for the cell)
    , tilemap : List (Maybe Int)

    -- World.lotEntries equivalent
    , lots : List { name : String, anchorCell : ( Int, Int ) }
    }


currentSavegameVersion : String
currentSavegameVersion =
    "1.0"



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
        [ ( "version", JE.string currentSavegameVersion )
        , ( "seed", encodeSeed world.seed )
        , ( "tilemapSize"
          , JE.object
                [ ( "width", JE.int tilemapConfig.horizontalCellsAmount )
                , ( "height", JE.int tilemapConfig.verticalCellsAmount )
                ]
          )
        , ( "tilemap", JE.list encodeMaybeTileId tilemapArray )
        , ( "lots"
          , JE.list
                (\{ name, anchorCell } ->
                    JE.object
                        [ ( "name", JE.string name )
                        , ( "anchorCell", encodeCellCoordinates anchorCell )
                        ]
                )
                lotsList
          )
        ]


extractTilemapTileIds : Tilemap -> List (Maybe Int)
extractTilemapTileIds tilemap =
    Tilemap.foldTiles
        (\_ tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    Just properties.id :: acc

                Tile.Buffer ->
                    -- Buffers should not exist upon saving, as savegames are created after WFC is solved.
                    Nothing :: acc

                _ ->
                    Nothing :: acc
        )
        []
        tilemap
        |> List.reverse


lotToSavegameData : World -> Lot -> Maybe { name : String, anchorCell : ( Int, Int ) }
lotToSavegameData world lot =
    findLotEntryCell world lot.id
        |> Maybe.map
            (\anchorCell ->
                { name = lot.name
                , anchorCell = anchorCell
                }
            )


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


encodeSeed : Random.Seed -> JE.Value
encodeSeed seed =
    seed
        -- TODO: consider what the seed value should be
        |> Random.step (Random.int Random.minInt Random.maxInt)
        |> Tuple.first
        |> String.fromInt
        |> JE.string


encodeMaybeTileId : Maybe Int -> JE.Value
encodeMaybeTileId maybeTileId =
    case maybeTileId of
        Just tileId ->
            JE.int tileId

        Nothing ->
            JE.null


encodeCellCoordinates : ( Int, Int ) -> JE.Value
encodeCellCoordinates ( x, y ) =
    JE.list JE.int [ x, y ]



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
        (JD.field "version" JD.string)
        (JD.field "seed" JD.string)
        (JD.field "tilemapSize" tilemapSizeDecoder)
        (JD.field "tilemap" (JD.list (JD.nullable JD.int)))
        (JD.field "lots" (JD.list lotDataDecoder))


tilemapSizeDecoder : JD.Decoder { width : Int, height : Int }
tilemapSizeDecoder =
    JD.map2 (\w h -> { width = w, height = h })
        (JD.field "width" JD.int)
        (JD.field "height" JD.int)


lotDataDecoder : JD.Decoder { name : String, anchorCell : ( Int, Int ) }
lotDataDecoder =
    JD.map2 (\name coords -> { name = name, anchorCell = coords })
        (JD.field "name" JD.string)
        (JD.field "anchorCell" cellCoordinatesDecoder)


cellCoordinatesDecoder : JD.Decoder ( Int, Int )
cellCoordinatesDecoder =
    JD.list JD.int
        |> JD.andThen
            (\coords ->
                case coords of
                    [ x, y ] ->
                        JD.succeed ( x, y )

                    _ ->
                        JD.fail "Expected array of 2 integers for cell coordinates"
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
        subgridConstraints =
            { horizontalCellsAmount = largeTile.width
            , verticalCellsAmount = largeTile.height
            }

        maybeTopLeftCell =
            Tile.largeTileTopLeftCell tilemapConfig drivewayCell largeTile.anchorIndex largeTile
    in
    case maybeTopLeftCell of
        Just topLeftCell ->
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


prepareLotMetadata : Tilemap.TilemapConfig -> { name : String, anchorCell : ( Int, Int ) } -> Result String LotMetadata
prepareLotMetadata tilemapConfig lotData =
    case ( Cell.fromCoordinates tilemapConfig lotData.anchorCell, Lots.findByName lotData.name ) of
        ( Nothing, _ ) ->
            Err ("Invalid lot entry cell coordinates: " ++ coordinatesToString lotData.anchorCell)

        ( _, Nothing ) ->
            Err ("Unknown lot name: " ++ lotData.name)

        ( Just entryCell, Just newLot ) ->
            case Cell.nextOrthogonalCell tilemapConfig newLot.entryDirection entryCell of
                Nothing ->
                    Err ("Cannot compute driveway cell for lot: " ++ lotData.name)

                Just drivewayCell ->
                    case findMatchingLargeTile newLot of
                        Nothing ->
                            Err ("No matching LargeTile found for lot: " ++ lotData.name)

                        Just largeTile ->
                            Ok
                                { name = lotData.name
                                , entryCell = lotData.anchorCell
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
        Err ("Unsupported savegame version: " ++ data.version)

    else
        let
            prepareLotMetadataResult seed =
                let
                    tilemapConfig =
                        { horizontalCellsAmount = data.tilemapSize.width
                        , verticalCellsAmount = data.tilemapSize.height
                        }
                in
                data.lots
                    |> List.map (prepareLotMetadata tilemapConfig)
                    |> combineResults
                    |> Result.map (\lotMetadataList -> ( seed, tilemapConfig, lotMetadataList ))

            restoreTilemapResult ( seed, tilemapConfig, lotMetadataList ) =
                restoreTilemap data.tilemap lotMetadataList tilemapConfig (World.empty seed tilemapConfig)
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


seedFromString : String -> Result String Random.Seed
seedFromString seedString =
    case String.toInt seedString of
        Just seedInt ->
            Ok (Random.initialSeed seedInt)

        Nothing ->
            Err ("Invalid seed value: " ++ seedString)


restoreTilemap : List (Maybe Int) -> List LotMetadata -> Tilemap.TilemapConfig -> World -> Result String World
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
                            Just (Just tileId) ->
                                let
                                    parentTileInfo =
                                        Cell.fromArray1DIndex tilemapConfig index
                                            |> Maybe.andThen (\cell -> Dict.get (Cell.coordinates cell) parentTilesDict)
                                in
                                createTileFromId tileId parentTileInfo

                            _ ->
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

                nextWorld =
                    { world | lots = nextLots }
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
