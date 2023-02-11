module Simulation.Zoning exposing (generateLot, removeInvalidLots)

import Collection exposing (Id)
import Data.Lots exposing (NewLot, allLots)
import Maybe.Extra as Maybe
import Model.Cell exposing (Cell)
import Model.Geometry exposing (oppositeOrthogonalDirection)
import Model.Lot as Lot exposing (Lot)
import Model.Tile as Tile
import Model.Tilemap as Tilemap
import Model.World as World exposing (World)
import Random
import Random.List
import Simulation.Infrastructure as Infrastructure
import Simulation.Traffic as Traffic
import Time


generateLot : Time.Posix -> World -> World
generateLot time world =
    let
        existingBuildingKinds =
            world.lots
                |> Collection.map (\_ lot -> lot.kind)
                |> Collection.values

        unusedLots =
            List.filter (\newLot -> not (List.member newLot.kind existingBuildingKinds)) allLots

        randomLot =
            unusedLots
                |> Random.List.choose
                |> Random.map Tuple.first

        ( potentialNewLot, nextSeed ) =
            Random.step randomLot world.seed

        updatedWorld =
            World.setSeed nextSeed world
    in
    potentialNewLot
        |> Maybe.andThen (attemptBuildLot time updatedWorld)
        |> Maybe.withDefault updatedWorld


attemptBuildLot : Time.Posix -> World -> NewLot -> Maybe World
attemptBuildLot time world newLot =
    let
        anchorOptions =
            world.tilemap
                |> Tilemap.toList
                    (\cell tile ->
                        if
                            Tile.isBasicRoad tile
                                && not (List.member newLot.drivewayExitDirection (Tile.potentialConnections tile))
                        then
                            validateAnchor newLot world cell

                        else
                            Nothing
                    )
                    Tilemap.NoFilter
                |> Maybe.values

        ( shuffledAnchors, _ ) =
            Random.step (Random.List.shuffle anchorOptions) world.seed
    in
    shuffledAnchors
        |> List.head
        |> Maybe.map
            (\anchor ->
                let
                    builderFn =
                        Lot.build newLot anchor

                    ( lot, nextLots ) =
                        Collection.addFromBuilder builderFn world.lots
                in
                world
                    |> World.refreshLots lot nextLots
                    |> World.setTilemap
                        (Tilemap.addAnchor anchor
                            lot.id
                            (oppositeOrthogonalDirection lot.drivewayExitDirection)
                            world.tilemap
                        )
                    |> Infrastructure.connectLotToRoadNetwork
                    |> Traffic.addLotResident time lot
            )


validateAnchor : NewLot -> World -> Cell -> Maybe Cell
validateAnchor newLot world anchor =
    let
        lotBoundingBox =
            Lot.constructionSite anchor newLot
    in
    if
        World.isEmptyArea lotBoundingBox world
            && not (Tilemap.hasAnchor world.tilemap anchor)
    then
        Just anchor

    else
        Nothing


removeInvalidLots : List Cell -> World -> World
removeInvalidLots changedCells world =
    let
        changedAnchors =
            List.filterMap
                (\cell ->
                    Tilemap.anchorAt world.tilemap cell
                        |> Maybe.map (Tuple.mapSecond (always cell))
                )
                changedCells
    in
    Collection.foldl
        (validateLot changedCells changedAnchors)
        world
        world.lots


validateLot : List Cell -> List ( Id, Cell ) -> Id -> Lot -> World -> World
validateLot changedCells changedAnchors lotId lot world =
    let
        lotOverlapsWithRoad =
            List.any (\cell -> Lot.inBounds cell lot) changedCells

        lotAnchorWasRemoved =
            List.any
                (\( id, cell ) ->
                    case Tilemap.tileAt world.tilemap cell of
                        Just tile ->
                            if id == lotId then
                                not (Tile.isLotEntry tile)

                            else
                                False

                        Nothing ->
                            id == lotId
                )
                changedAnchors
    in
    if lotAnchorWasRemoved || lotOverlapsWithRoad then
        World.removeLot lotId world

    else
        world
