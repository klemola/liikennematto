module Simulation.Zoning exposing (generateLot, validateLots)

import Data.Lots exposing (allLots)
import Dict
import Maybe.Extra as Maybe
import Model.Geometry exposing (orthogonalDirections)
import Model.Lot as Lot exposing (Anchor, Lot, NewLot)
import Model.Tile as Tile
import Model.Tilemap as Tilemap exposing (Cell, Tilemap)
import Model.World as World exposing (World)
import Random
import Random.List
import Simulation.Infrastructure as Infrastructure
import Simulation.Traffic as Traffic


generateLot : World -> Random.Seed -> ( World, Random.Seed )
generateLot world seed =
    let
        existingBuildingKinds =
            world.lots
                |> Dict.map (\_ lot -> lot.content.kind)
                |> Dict.values

        unusedLots =
            List.filter (\{ content } -> not (List.member content.kind existingBuildingKinds)) allLots

        randomLot =
            unusedLots
                |> Random.List.choose
                |> Random.map Tuple.first

        ( potentialNewLot, nextSeed ) =
            Random.step randomLot seed

        nextWorld =
            potentialNewLot
                |> Maybe.andThen (attemptBuildLot world seed)
                |> Maybe.map (addLot world)
                |> Maybe.withDefault world
    in
    ( nextWorld, nextSeed )


attemptBuildLot : World -> Random.Seed -> NewLot -> Maybe Lot
attemptBuildLot world seed newLot =
    let
        anchors =
            world.tilemap
                |> Tilemap.toList
                    (\cell tile ->
                        if
                            Tile.isBasicRoad tile
                                && not (List.member newLot.content.entryDirection (Tile.potentialConnections tile))
                        then
                            Lot.createAnchor newLot cell
                                |> Maybe.andThen (validateAnchor newLot world)

                        else
                            Nothing
                    )
                |> Maybe.values

        ( shuffledAnchors, _ ) =
            Random.step (Random.List.shuffle anchors) seed
    in
    shuffledAnchors
        |> List.head
        |> Maybe.map (Lot.build newLot)


validateAnchor : NewLot -> World -> Anchor -> Maybe Anchor
validateAnchor newLot world anchor =
    let
        lotBoundingBox =
            Lot.newLotBuildArea anchor newLot
    in
    if
        World.isEmptyArea lotBoundingBox world
            && not (World.hasLotAnchor anchor.anchorCell world)
            && isNotAtTheEndOfARoad world anchor
    then
        Just anchor

    else
        Nothing


isNotAtTheEndOfARoad : World -> Anchor -> Bool
isNotAtTheEndOfARoad world { anchorCell } =
    orthogonalDirections
        |> List.all
            (\orthogonalDirection ->
                case
                    Tilemap.nextOrthogonalCell orthogonalDirection anchorCell
                        |> Maybe.andThen (Tilemap.tileAt world.tilemap)
                of
                    Just neighbor ->
                        Tile.isBasicRoad neighbor

                    Nothing ->
                        True
            )


addLot : World -> Lot -> World
addLot world lot =
    let
        ( lotId, worldWithlot ) =
            World.addLot lot world
    in
    worldWithlot
        |> Infrastructure.connectLotToRoadNetwork
        |> Traffic.addLotResident lotId lot


validateLots : List Cell -> World -> World
validateLots changedCells world =
    Dict.foldl
        (\lotId lot acc ->
            if
                hasValidAnchorCell world.tilemap lot
                    && List.all (\cell -> not (Lot.inBounds cell lot)) changedCells
            then
                acc

            else
                World.removeLot lotId acc
        )
        world
        world.lots


hasValidAnchorCell : Tilemap -> Lot -> Bool
hasValidAnchorCell tilemap lot =
    case Tilemap.tileAt tilemap lot.anchor.anchorCell of
        Just tile ->
            Tile.isBasicRoad tile

        Nothing ->
            False
