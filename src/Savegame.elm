module Savegame exposing
    ( SavegameData
    , decode
    , encode
    , spawnLotResidents
    )

import Array
import Data.Lots exposing (NewLot, allLots)
import Data.TileSet
import Dict
import Json.Decode as JD
import Json.Encode as JE
import Lib.Collection as Collection
import Model.World as World exposing (World)
import Random
import Simulation.Lot as Lot exposing (Lot)
import Simulation.Traffic
import Tilemap.Cell as Cell
import Tilemap.Core as Tilemap exposing (Tilemap)
import Tilemap.Tile as Tile
import Time


type alias SavegameData =
    { version : String
    , seed : String
    , tilemapSize : { width : Int, height : Int }

    -- 1D array of tile IDs (null if not tile has been set for the cell)
    , tilemap : List (Maybe Int)

    -- Raw data for a lookup map to detect cells that are part of a large tile
    , parentTiles : List ( Int, Int, Int )

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

        parentTilesList =
            extractParentTilesInfo world.tilemap

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
        , ( "parentTiles", JE.list encodeParentTileInfo parentTilesList )
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


extractParentTilesInfo : Tilemap -> List ( Int, Int, Int )
extractParentTilesInfo tilemap =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig tilemap
    in
    Tilemap.foldTiles
        (\cell tile acc ->
            case tile.kind of
                Tile.Fixed properties ->
                    case properties.parentTile of
                        Just ( parentTileId, subgridIndex ) ->
                            let
                                tileIndex =
                                    Cell.array1DIndex tilemapConfig cell
                            in
                            ( tileIndex, parentTileId, subgridIndex ) :: acc

                        Nothing ->
                            acc

                _ ->
                    acc
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


encodeParentTileInfo : ( Int, Int, Int ) -> JE.Value
encodeParentTileInfo ( tileIndex, parentTileId, subgridIndex ) =
    JE.list JE.int [ tileIndex, parentTileId, subgridIndex ]



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
    JD.map6 SavegameData
        (JD.field "version" JD.string)
        (JD.field "seed" JD.string)
        (JD.field "tilemapSize" tilemapSizeDecoder)
        (JD.field "tilemap" (JD.list (JD.nullable JD.int)))
        (JD.field "parentTiles" (JD.list parentTileInfoDecoder))
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


parentTileInfoDecoder : JD.Decoder ( Int, Int, Int )
parentTileInfoDecoder =
    JD.list JD.int
        |> JD.andThen
            (\values ->
                case values of
                    [ tileIndex, parentTileId, subgridIndex ] ->
                        JD.succeed ( tileIndex, parentTileId, subgridIndex )

                    _ ->
                        JD.fail "Expected array of 3 integers for parent tile info"
            )



--
-- World reconstruction
--


worldFromSavegameData : SavegameData -> Result String World
worldFromSavegameData data =
    if data.version /= currentSavegameVersion then
        Err ("Unsupported savegame version: " ++ data.version)

    else
        seedFromString data.seed
            |> Result.andThen
                (\seed ->
                    let
                        tilemapConfig =
                            { horizontalCellsAmount = data.tilemapSize.width
                            , verticalCellsAmount = data.tilemapSize.height
                            }

                        emptyWorld =
                            World.empty seed tilemapConfig
                    in
                    restoreTilemap data.tilemap data.parentTiles tilemapConfig emptyWorld
                        |> Result.andThen (restoreLots data.lots)
                        |> Result.map World.updateRoadNetwork
                )


seedFromString : String -> Result String Random.Seed
seedFromString seedString =
    case String.toInt seedString of
        Just seedInt ->
            Ok (Random.initialSeed seedInt)

        Nothing ->
            Err ("Invalid seed value: " ++ seedString)


restoreTilemap : List (Maybe Int) -> List ( Int, Int, Int ) -> Tilemap.TilemapConfig -> World -> Result String World
restoreTilemap tileIds parentTilesInfo tilemapConfig world =
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
                buildParentTilesDict parentTilesInfo

            tilemap =
                Tilemap.createTilemap tilemapConfig
                    (\index ->
                        case Array.get index tilemapArray of
                            Just (Just tileId) ->
                                let
                                    parentTileInfo =
                                        Dict.get index parentTilesDict
                                in
                                createTileFromId tileId parentTileInfo

                            _ ->
                                Tile.init Tile.Unintialized
                    )
        in
        Ok (World.setTilemap tilemap world)


buildParentTilesDict : List ( Int, Int, Int ) -> Dict.Dict Int ( Int, Int )
buildParentTilesDict parentTilesInfo =
    List.foldl
        (\( tileIndex, parentTileId, subgridIndex ) dict ->
            Dict.insert tileIndex ( parentTileId, subgridIndex ) dict
        )
        Dict.empty
        parentTilesInfo


createTileFromId : Int -> Maybe ( Int, Int ) -> Tile.Tile
createTileFromId tileId parentTileInfo =
    let
        tileConfig =
            Data.TileSet.tileById tileId

        ( tile, _ ) =
            Tile.fromTileConfig tileConfig parentTileInfo Tile.AddFromSaveGame
    in
    tile


restoreLots : List { name : String, anchorCell : ( Int, Int ) } -> World -> Result String World
restoreLots lotsData world =
    List.foldl
        (\lotData ->
            Result.andThen (restoreSingleLot lotData)
        )
        (Ok world)
        lotsData


restoreSingleLot : { name : String, anchorCell : ( Int, Int ) } -> World -> Result String World
restoreSingleLot lotData world =
    let
        tilemapConfig =
            Tilemap.getTilemapConfig world.tilemap

        maybeAnchorCell =
            Cell.fromCoordinates tilemapConfig lotData.anchorCell

        maybeNewLot =
            findLotByName lotData.name
    in
    case ( maybeAnchorCell, maybeNewLot ) of
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
                ("Invalid lot anchor cell coordinates: "
                    ++ Debug.toString lotData.anchorCell
                )

        ( _, Nothing ) ->
            Err ("Unknown lot name: " ++ lotData.name)


findLotByName : String -> Maybe NewLot
findLotByName name =
    allLots
        |> List.filter (\lot -> lot.name == name)
        |> List.head



--
-- Resident spawning
--


spawnLotResidents : Time.Posix -> World -> World
spawnLotResidents currentTime world =
    Collection.foldl
        (\lotId lot updatedWorld ->
            case findLotByName lot.name of
                Just newLot ->
                    Simulation.Traffic.addLotResidents currentTime lotId newLot.residents updatedWorld

                Nothing ->
                    updatedWorld
        )
        world
        world.lots
