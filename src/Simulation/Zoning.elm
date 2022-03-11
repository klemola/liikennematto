module Simulation.Zoning exposing (generateLot, removeInvalidLots)

import Data.Lots exposing (allLots)
import Dict
import Maybe.Extra as Maybe
import Model.Cell as Cell exposing (Cell)
import Model.Entity as Entity exposing (Id)
import Model.Geometry exposing (oppositeOrthogonalDirection, orthogonalDirections)
import Model.Lot as Lot exposing (Lot, NewLot)
import Model.Tile as Tile
import Model.Tilemap as Tilemap
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
                |> Dict.map (\_ lot -> lot.kind)
                |> Dict.values

        unusedLots =
            List.filter (\newLot -> not (List.member newLot.kind existingBuildingKinds)) allLots

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


attemptBuildLot : World -> Random.Seed -> NewLot -> Maybe ( Lot, Cell )
attemptBuildLot world seed newLot =
    let
        anchorOptions =
            world.tilemap
                |> Tilemap.toList
                    (\cell tile ->
                        if
                            Tile.isBasicRoad tile
                                && not (List.member newLot.drivewayExit (Tile.potentialConnections tile))
                        then
                            validateAnchor newLot world cell

                        else
                            Nothing
                    )
                |> Maybe.values

        ( shuffledAnchors, _ ) =
            Random.step (Random.List.shuffle anchorOptions) seed

        nextLotId =
            Entity.nextId world.lots
    in
    shuffledAnchors
        |> List.head
        |> Maybe.map
            (\anchor ->
                ( Lot.build nextLotId newLot anchor, anchor )
            )


validateAnchor : NewLot -> World -> Cell -> Maybe Cell
validateAnchor newLot world anchor =
    let
        lotBoundingBox =
            Lot.newLotBuildArea anchor newLot
    in
    if
        World.isEmptyArea lotBoundingBox world
            && not (Tilemap.hasAnchor world.tilemap anchor)
            && isNotAtTheEndOfARoad world anchor
    then
        Just anchor

    else
        Nothing


isNotAtTheEndOfARoad : World -> Cell -> Bool
isNotAtTheEndOfARoad world anchor =
    orthogonalDirections
        |> List.all
            (\orthogonalDirection ->
                case
                    Cell.nextOrthogonalCell orthogonalDirection anchor
                        |> Maybe.andThen (Tilemap.tileAt world.tilemap)
                of
                    Just neighbor ->
                        Tile.isBasicRoad neighbor

                    Nothing ->
                        True
            )


addLot : World -> ( Lot, Cell ) -> World
addLot world ( lot, anchor ) =
    let
        worldWithlot =
            { world
                | lots = Dict.insert lot.id lot world.lots
                , tilemap =
                    Tilemap.addAnchor anchor
                        lot.id
                        (oppositeOrthogonalDirection lot.drivewayExit)
                        world.tilemap
            }
    in
    worldWithlot
        |> Infrastructure.connectLotToRoadNetwork
        |> Traffic.addLotResident lot.id lot


removeInvalidLots : List Cell -> World -> World
removeInvalidLots changedCells world =
    Dict.foldl
        (validateLot changedCells)
        world
        world.lots


validateLot : List Cell -> Id -> Lot -> World -> World
validateLot changedCells lotId lot world =
    let
        changedAnchors =
            List.filterMap (Tilemap.anchorAt world.tilemap)
                changedCells

        -- Room for improvement: add a QuadTree lookup for lots and remove lots based on Cell BB overlap
        lotOverlapsWithRoad =
            List.any (\cell -> Lot.inBounds cell lot) changedCells

        lotAnchorWasRemoved =
            List.any
                (\( id, _, tile ) ->
                    if id == lot.id then
                        not (Tile.isLotEntry tile)

                    else
                        False
                )
                changedAnchors
    in
    if lotOverlapsWithRoad || lotAnchorWasRemoved then
        World.removeLot lotId world

    else
        world
