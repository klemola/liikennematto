module Editor.WFC exposing
    ( Model
    , currentCell
    , failed
    , init
    , propagate
    , propagateN
    , propagationContextDebug
    , solve
    , stateDebug
    , stopped
    , toTilemap
    )

import Array
import Common exposing (attemptFoldList, attemptMapList)
import Data.TileSet exposing (allTilesAndMetaTiles, defaultTile, pairingsForSocket)
import List.Extra
import List.Nonempty exposing (Nonempty)
import Model.Cell as Cell exposing (Cell)
import Model.Geometry
    exposing
        ( OrthogonalDirection
        , oppositeOrthogonalDirection
        , orthogonalDirectionToString
        , orthogonalDirections
        )
import Model.Tile as Tile exposing (Tile, TileKind(..))
import Model.TileConfig as TileConfig
    exposing
        ( LargeTile
        , SingleTile
        , Socket
        , TileConfig
        , TileId
        , socketByDirection
        , socketByDirectionWithConfig
        , tileConfigId
        )
import Model.Tilemap as Tilemap exposing (Tilemap, TilemapConfig)
import Random exposing (Seed)


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , currentCell : Maybe Cell
    , targetCell : Maybe Cell
    , propagationDirection : Maybe OrthogonalDirection
    , openSteps : List PropagationStep
    , supervisorState : SupervisorState
    , seed : Seed
    }


type SupervisorState
    = Propagating
    | Done
    | Recovering PropagationFailure
    | Failed PropagationFailure


type PropagationStep
    = PickTile Cell TileConfig
    | KeepMatching Cell Cell


type PropagationFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidBigTilePlacement Cell LargeTile
    | InvalidDirection
    | NoCandidates
    | TileNotFound


type alias Candidate =
    { cell : Cell
    , options : Nonempty TileId
    }


init : TilemapConfig -> Model
init tilemapConfig =
    Model
        { tilemap = Tilemap.empty tilemapConfig
        , currentCell = Nothing
        , targetCell = Nothing
        , propagationDirection = Nothing
        , openSteps = []
        , supervisorState = Propagating
        , seed = tilemapConfig.initialSeed
        }


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position.
-}
solve : TilemapConfig -> Model
solve tilemapConfig =
    let
        nextModel =
            propagate <| init tilemapConfig
    in
    solve_ nextModel


solve_ : Model -> Model
solve_ model =
    if stopped model then
        model

    else
        solve_ (propagate model)


propagate : Model -> Model
propagate model =
    let
        ((Model modelDetails) as afterPropagation) =
            propagate_ model
    in
    case modelDetails.supervisorState of
        Recovering propagationFailure ->
            case propagationFailure of
                NoPotentialMatch ->
                    -- Aknowledge failure, can resume propagation
                    Model { modelDetails | supervisorState = Propagating }

                InvalidBigTilePlacement cell largeTile ->
                    case Tilemap.removeSuperpositionOption cell largeTile.id modelDetails.tilemap of
                        Ok tilemapWithRecovery ->
                            Model
                                { modelDetails
                                    | supervisorState = Propagating
                                    , tilemap = tilemapWithRecovery
                                }

                        Err _ ->
                            Model { modelDetails | supervisorState = Failed propagationFailure }

                -- No recovery strategy yet
                _ ->
                    Model { modelDetails | supervisorState = Failed propagationFailure }

        _ ->
            afterPropagation


propagateN : Int -> Model -> Model
propagateN nTimes model =
    if nTimes == 0 then
        model

    else
        let
            propagationResult =
                propagate model
        in
        if failed propagationResult then
            propagationResult

        else
            propagateN (nTimes - 1) propagationResult


{-| Execute a single step. This can mean picking the next random tile
or propagating restrictions resulting from the last placement of a tile.
-}
propagate_ : Model -> Model
propagate_ (Model ({ openSteps, tilemap } as modelDetails)) =
    case openSteps of
        step :: otherSteps ->
            let
                processStepResult =
                    processStep tilemap step

                position =
                    stepPosition step

                withPosition =
                    { modelDetails
                        | currentCell = position.currentCell
                        , targetCell = position.targetCell
                        , propagationDirection = position.propagationDirection
                    }

                nextModelDetails =
                    case processStepResult of
                        Ok ( additionalSteps, nextTilemap ) ->
                            { withPosition
                                | tilemap = nextTilemap
                                , supervisorState = Propagating
                                , openSteps = otherSteps ++ additionalSteps
                            }

                        Err propagationFailure ->
                            { withPosition
                                | supervisorState = Recovering propagationFailure
                                , openSteps = otherSteps
                            }
            in
            Model nextModelDetails

        [] ->
            let
                nextModelDetails =
                    if not (propagationDone modelDetails) then
                        let
                            withPick =
                                pickRandom modelDetails
                        in
                        { withPick | supervisorState = Propagating }

                    else
                        { modelDetails
                            | supervisorState = Done
                            , currentCell = Nothing
                            , targetCell = Nothing
                            , propagationDirection = Nothing
                        }
            in
            Model nextModelDetails



--
-- Propagation
--


tileConfigById : Tilemap -> Int -> TileConfig
tileConfigById tilemap tileId =
    -- TODO: optimize?
    -- TODO: fix: hardcoded tileset, dangerous default tile
    allTilesAndMetaTiles
        |> List.Extra.find (\tileConfig -> tileConfigId tileConfig == tileId)
        |> Maybe.withDefault defaultTile


stepPosition :
    PropagationStep
    ->
        { currentCell : Maybe Cell
        , targetCell : Maybe Cell
        , propagationDirection : Maybe OrthogonalDirection
        }
stepPosition step =
    case step of
        PickTile cell _ ->
            { currentCell = Just cell, targetCell = Nothing, propagationDirection = Nothing }

        KeepMatching cellA cellB ->
            { currentCell = Just cellA
            , targetCell = Just cellB
            , propagationDirection = Cell.orthogonalDirection cellA cellB
            }


matchingSuperpositionOptions : Tilemap -> OrthogonalDirection -> TileId -> List TileId -> List TileId
matchingSuperpositionOptions tilemap dir originTileId options =
    let
        originTileConfig =
            tileConfigById tilemap originTileId

        originSocket =
            socketByDirectionWithConfig originTileConfig dir
    in
    List.filter
        (canDock
            tilemap
            (oppositeOrthogonalDirection dir)
            originSocket
        )
        options


canDock : Tilemap -> OrthogonalDirection -> Socket -> Int -> Bool
canDock tilemap dockDir dockSocket dockTileId =
    let
        dockTileConfig =
            tileConfigById tilemap dockTileId

        matchSocket =
            socketByDirectionWithConfig dockTileConfig dockDir

        pairings =
            pairingsForSocket dockSocket
    in
    List.member matchSocket pairings


processStep : Tilemap -> PropagationStep -> Result PropagationFailure ( List PropagationStep, Tilemap )
processStep tilemap step =
    case step of
        PickTile cell tileConfig ->
            case tileConfig of
                TileConfig.Large largeTile ->
                    attemptPlaceLargeTile tilemap cell largeTile

                TileConfig.Single singleTile ->
                    let
                        tilemapConfig =
                            Tilemap.config tilemap

                        stepInDirection dir =
                            Cell.nextOrthogonalCell tilemapConfig dir cell
                                |> Maybe.map (KeepMatching cell)

                        nextSteps =
                            List.filterMap stepInDirection orthogonalDirections

                        -- TODO: ignoring actions
                        ( tile, _ ) =
                            Tile.new singleTile.id Tile.BuildInstantly
                    in
                    Ok
                        ( nextSteps
                        , Tilemap.setTile cell tile tilemap
                        )

        KeepMatching from to ->
            Maybe.map2 Tuple.pair
                (Tilemap.tileAtAny tilemap from)
                (Tilemap.tileAtAny tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (updateSuperpositionOptions tilemap from to)
                |> Result.map (Tuple.pair [])


updateSuperpositionOptions : Tilemap -> Cell -> Cell -> ( Tile, Tile ) -> Result PropagationFailure Tilemap
updateSuperpositionOptions tilemap from to tilePair =
    Cell.orthogonalDirection from to
        |> Result.fromMaybe InvalidDirection
        |> Result.andThen
            (superpositionOptionsForTile
                tilemap
                tilePair
            )
        |> Result.map
            (\superpositionOptions ->
                Tilemap.setSuperpositionOptions to superpositionOptions tilemap
            )


superpositionOptionsForTile :
    Tilemap
    -> ( Tile, Tile )
    -> OrthogonalDirection
    -> Result PropagationFailure (List Int)
superpositionOptionsForTile tilemap ( originTile, targetTile ) dir =
    case ( originTile.kind, targetTile.kind ) of
        ( Fixed originTileId, Superposition options ) ->
            let
                revisedOptions =
                    matchingSuperpositionOptions tilemap dir originTileId options
            in
            if List.isEmpty revisedOptions then
                Err NoSuperpositionOptions

            else
                Ok revisedOptions

        _ ->
            Err NoPotentialMatch


type alias NeighborUpdateContext =
    { dir : OrthogonalDirection
    , neighborTile : Tile
    , neighborCell : Cell
    , tilemap : Tilemap
    , steps : List PropagationStep
    }


applyToNeighbor :
    (NeighborUpdateContext -> TileId -> Result error ( List PropagationStep, Tilemap ))
    -> (NeighborUpdateContext -> List TileId -> Result error ( List PropagationStep, Tilemap ))
    -> Cell
    -> Tilemap
    -> Result error ( List PropagationStep, Tilemap )
applyToNeighbor onFixed onSuperposition cell tilemap =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    attemptFoldList
        (\dir ( steps, nextTilemap ) ->
            case Cell.nextOrthogonalCell tilemapConfig dir cell of
                Just toCell ->
                    case Tilemap.tileAtAny nextTilemap toCell of
                        Just toTile ->
                            let
                                neighborUpdateContext =
                                    NeighborUpdateContext dir toTile toCell nextTilemap steps
                            in
                            case toTile.kind of
                                Fixed tileId ->
                                    onFixed neighborUpdateContext tileId

                                Superposition options ->
                                    onSuperposition neighborUpdateContext options

                        Nothing ->
                            Ok ( steps, nextTilemap )

                Nothing ->
                    Ok ( steps, nextTilemap )
        )
        ( [], tilemap )
        orthogonalDirections


{-| Returns true if all positions in the grid have a tile assigned
-}
propagationDone : ModelDetails -> Bool
propagationDone { tilemap, openSteps } =
    List.isEmpty openSteps && Tilemap.all propagatinDonePredicate tilemap


propagatinDonePredicate : Tile -> Bool
propagatinDonePredicate tile =
    case tile.kind of
        Superposition _ ->
            False

        Fixed _ ->
            True



--
-- Random pick
--


pickRandom : ModelDetails -> ModelDetails
pickRandom ({ openSteps, tilemap, seed } as modelDetails) =
    case
        List.Nonempty.fromList (nextCandidates tilemap)
    of
        Nothing ->
            { modelDetails | supervisorState = Recovering NoCandidates }

        Just candidates ->
            let
                ( randomCandidate, seedAfterCandidateGen ) =
                    Random.step (List.Nonempty.sample candidates) seed

                randomOptionGen =
                    toWeightedOptions tilemap randomCandidate.options

                ( randomOption, nextSeed ) =
                    Random.step randomOptionGen seedAfterCandidateGen

                pickRandomStep =
                    [ PickTile randomCandidate.cell randomOption ]
            in
            { modelDetails
                | openSteps = pickRandomStep ++ openSteps
                , currentCell = Nothing
                , targetCell = Nothing
                , propagationDirection = Nothing
                , seed = nextSeed
            }


toWeightedOptions : Tilemap -> Nonempty TileId -> Random.Generator TileConfig
toWeightedOptions tilemap options =
    let
        withWeights =
            List.Nonempty.map
                (\option ->
                    let
                        tileConfig =
                            tileConfigById tilemap option
                    in
                    ( 1.0 - TileConfig.complexity tileConfig, tileConfig )
                )
                options
    in
    Random.weighted (List.Nonempty.head withWeights) (List.Nonempty.toList withWeights)


nextCandidates : Tilemap -> List Candidate
nextCandidates tilemap =
    let
        tilemapConfig =
            Tilemap.config tilemap

        toCandidate cell tile ( candidates, minEntropy ) =
            case tile.kind of
                Fixed _ ->
                    ( candidates, minEntropy )

                Superposition options ->
                    case List.Nonempty.fromList options of
                        Just nonemptyOptions ->
                            let
                                currentEntropy =
                                    List.Nonempty.length nonemptyOptions
                            in
                            if currentEntropy > minEntropy then
                                ( candidates, minEntropy )

                            else if currentEntropy < minEntropy then
                                ( [ { cell = cell, options = nonemptyOptions } ]
                                , currentEntropy
                                )

                            else
                                ( { cell = cell, options = nonemptyOptions } :: candidates
                                , currentEntropy
                                )

                        Nothing ->
                            ( candidates, minEntropy )
    in
    tilemap
        |> Tilemap.fold toCandidate ( [], List.length tilemapConfig.tiles + 1 )
        |> Tuple.first



--
-- Extra constraints
--


attemptPlaceLargeTile : Tilemap -> Cell -> LargeTile -> Result PropagationFailure ( List PropagationStep, Tilemap )
attemptPlaceLargeTile tilemap anchorCell tile =
    let
        tilemapConstraints =
            Tilemap.config tilemap

        -- The large tile is a subgrid in the main grid; the tilemap
        subgridDimensions =
            { horizontalCellsAmount = tile.width
            , verticalCellsAmount = tile.height
            }

        -- The local (subgrid) cell of the anchor tile
        subgridAnchorCell =
            Cell.fromArray1DIndex subgridDimensions tile.anchorIndex

        -- Find the cell of the top left cell (index 0) of the large tile subgrid,
        -- but in the space of the tilemap (local to global coordinates)
        topLeftCornerCell =
            subgridAnchorCell
                |> Maybe.map Cell.coordinates
                |> Maybe.andThen
                    (\( x, y ) ->
                        Cell.translateBy tilemapConstraints ( -x + 1, -y + 1 ) anchorCell
                    )
    in
    topLeftCornerCell
        |> Result.fromMaybe "Invalid origin"
        |> Result.andThen
            (\origin ->
                tile.tiles
                    |> Array.toIndexedList
                    |> attemptMapList
                        (\( subgridIdx, singleTile ) ->
                            let
                                cellInGlobalCoordinates =
                                    subgridIdx
                                        -- Use of unsafe function: we know that here that the Cell can be constructed
                                        |> Cell.fromArray1DIndexUnsafe subgridDimensions
                                        |> Cell.placeIn tilemapConstraints origin
                            in
                            case cellInGlobalCoordinates of
                                Just cell ->
                                    Ok ( cell, singleTile )

                                Nothing ->
                                    Err "Cannot translate cell to global coordinates"
                        )
            )
        |> Result.andThen (attemptPlaceLargeTileHelper [] tilemap)
        |> Result.mapError (\_ -> InvalidBigTilePlacement anchorCell tile)


attemptPlaceLargeTileHelper : List PropagationStep -> Tilemap -> List ( Cell, SingleTile ) -> Result String ( List PropagationStep, Tilemap )
attemptPlaceLargeTileHelper steps tilemap tileList =
    case tileList of
        [] ->
            Ok ( steps, tilemap )

        ( cell, currentTile ) :: remainingTiles ->
            let
                nextTilemapResult =
                    tilemap
                        |> attemptSubTileNeighborUpdate currentTile cell
                        |> Result.map
                            (\( nextSteps, tilemapWithNeighborUpdate ) ->
                                let
                                    -- TODO: ignoring actions
                                    ( tile, _ ) =
                                        Tile.new currentTile.id Tile.BuildInstantly
                                in
                                ( nextSteps, Tilemap.setTile cell tile tilemapWithNeighborUpdate )
                            )
            in
            case nextTilemapResult of
                Err _ ->
                    nextTilemapResult

                Ok ( nextSteps, nextTilemap ) ->
                    attemptPlaceLargeTileHelper (steps ++ nextSteps) nextTilemap remainingTiles


attemptSubTileNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result String ( List PropagationStep, Tilemap )
attemptSubTileNeighborUpdate currentTile originCell tilemap =
    applyToNeighbor
        (\nuCtx neighborTileId ->
            let
                originSocket =
                    socketByDirection currentTile.sockets nuCtx.dir
            in
            if canDock nuCtx.tilemap (oppositeOrthogonalDirection nuCtx.dir) originSocket neighborTileId then
                -- Tiles can dock, no tilemap update needed = skip
                Ok ( nuCtx.steps, nuCtx.tilemap )

            else
                Err "Can't dock fixed neighbor"
        )
        (\nuCtx _ ->
            Ok
                ( KeepMatching originCell nuCtx.neighborCell :: nuCtx.steps
                , nuCtx.tilemap
                )
        )
        originCell
        tilemap



--
-- Interop, queries and debug
--


stopped : Model -> Bool
stopped (Model modelDetails) =
    modelDetails.supervisorState /= Propagating


failed : Model -> Bool
failed (Model modelDetails) =
    case modelDetails.supervisorState of
        Failed _ ->
            True

        _ ->
            False


currentCell : Model -> Maybe Cell
currentCell (Model modelDetails) =
    modelDetails.currentCell


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


stateDebug : Model -> String
stateDebug (Model modelDetails) =
    case modelDetails.supervisorState of
        Propagating ->
            "propagating"

        Recovering propagationFailure ->
            String.join " "
                [ "recovering:"
                , propagationFailureToString propagationFailure
                ]

        Failed propagationFailure ->
            String.join " "
                [ "failed:"
                , propagationFailureToString propagationFailure
                ]

        Done ->
            "done"


propagationFailureToString : PropagationFailure -> String
propagationFailureToString propagationFailure =
    case propagationFailure of
        NoSuperpositionOptions ->
            "No superposition opts"

        NoPotentialMatch ->
            "No potential match"

        InvalidBigTilePlacement _ _ ->
            "Invalid big tile surroundings (no space or socket mismatch)"

        InvalidDirection ->
            "Invalid direction (cell to cell)"

        TileNotFound ->
            "Tile not found"

        NoCandidates ->
            "No candidates available"


propagationContextDebug :
    Model
    ->
        { position : List String
        , openSteps : List String
        }
propagationContextDebug (Model modelDetails) =
    { position =
        [ Maybe.map Cell.toString modelDetails.currentCell |> Maybe.withDefault "-"
        , Maybe.map Cell.toString modelDetails.targetCell |> Maybe.withDefault "-"
        , Maybe.map orthogonalDirectionToString modelDetails.propagationDirection |> Maybe.withDefault "-"
        ]
    , openSteps = List.map stepDebug modelDetails.openSteps
    }


stepDebug : PropagationStep -> String
stepDebug step =
    case step of
        PickTile cell tileConfig ->
            String.join " "
                [ "PickTile"
                , Cell.toString cell
                , "tile config:"
                , TileConfig.toString tileConfig
                ]

        KeepMatching fromCell toCell ->
            String.join " " [ "KeepMatching from:", Cell.toString fromCell, "to:", Cell.toString toCell ]
