module Tilemap.TileInventory exposing
    ( TileInventory
    , chooseRandom
    , countAvailable
    , increaseCount
    , isAvailable
    , markAsUsed
    , restoreItem
    , tileIdItemPairs
    )

import Dict exposing (Dict)
import Random
import Random.List
import Tilemap.TileConfig exposing (TileId)


type alias TileInventory a =
    Dict TileId a



--
-- Keep track of tile count
--


countAvailable : TileInventory (List a) -> TileInventory Int
countAvailable =
    Dict.map (\_ items -> List.length items)


markAsUsed : TileId -> TileInventory Int -> TileInventory Int
markAsUsed tileId =
    Dict.update tileId (Maybe.map (\numRemaining -> max (numRemaining - 1) 0))


increaseCount : TileId -> TileInventory Int -> TileInventory Int
increaseCount tileId =
    Dict.update tileId (Maybe.map (\numRemaining -> max numRemaining 0 + 1))


isAvailable : TileId -> TileInventory Int -> Bool
isAvailable tileId inventory =
    case Dict.get tileId inventory of
        Just count ->
            count > 0

        Nothing ->
            -- If there is no inventory entry for the tileId, it is always available
            True



--
-- Keep track of individual tiles
--


chooseRandom : TileId -> Random.Seed -> TileInventory (List a) -> ( ( Maybe a, List a ), Random.Seed )
chooseRandom tileId seed inventory =
    let
        options =
            Dict.get tileId inventory
                |> Maybe.withDefault []
    in
    Random.step (Random.List.choose options) seed


restoreItem : (( TileId, a ) -> Bool) -> List ( TileId, a ) -> TileInventory (List a) -> TileInventory (List a)
restoreItem matcher lookup inventory =
    case
        lookup
            |> List.filter matcher
            |> List.head
    of
        Just ( tileId, match ) ->
            Dict.update tileId
                (Maybe.map (\items -> match :: items))
                inventory

        Nothing ->
            inventory


tileIdItemPairs : TileInventory (List a) -> List ( TileId, a )
tileIdItemPairs inventory =
    Dict.foldl
        (\tileId items pairs ->
            pairs ++ List.map (Tuple.pair tileId) items
        )
        []
        inventory
