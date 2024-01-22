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
import Stack exposing (Stack)


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , currentCell : Maybe Cell
    , targetCell : Maybe Cell
    , propagationDirection : Maybe OrthogonalDirection
    , openSteps : List PropagationStep
    , previousSteps : PreviousSteps
    , supervisorState : SupervisorState
    , seed : Seed
    }


type PropagationStep
    = PickTile Cell TileConfig
    | PlaceSubgridTile Cell SingleTile TileId
    | KeepMatching Cell Cell


type alias PreviousSteps =
    Stack ( PropagationStep, Nonempty TileId )


type SupervisorState
    = Propagating
    | Done
    | Recovering PropagationFailure
    | Failed PropagationFailure


type PropagationFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidBigTilePlacement Cell TileId
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
        , previousSteps = Stack.empty
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

                InvalidBigTilePlacement cell largeTileId ->
                    case Tilemap.removeSuperpositionOption cell largeTileId modelDetails.tilemap of
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
propagate_ (Model ({ openSteps, previousSteps, tilemap } as modelDetails)) =
    case openSteps of
        step :: otherSteps ->
            let
                position =
                    stepPosition step

                withPosition =
                    { modelDetails
                        | currentCell = position.currentCell
                        , targetCell = position.targetCell
                        , propagationDirection = position.propagationDirection
                    }
            in
            case processStep tilemap step of
                Ok ( additionalSteps, nextTilemap ) ->
                    Model
                        { withPosition
                            | tilemap = nextTilemap
                            , supervisorState = Propagating
                            , openSteps = otherSteps ++ additionalSteps
                            , previousSteps =
                                case stepSuperposition tilemap step of
                                    Just superpositionOptions ->
                                        Stack.push ( step, superpositionOptions ) previousSteps

                                    Nothing ->
                                        previousSteps
                        }

                Err propagationFailure ->
                    Model
                        { withPosition
                            | supervisorState = Recovering propagationFailure
                            , openSteps = otherSteps
                        }

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


stepSuperposition : Tilemap -> PropagationStep -> Maybe (Nonempty TileId)
stepSuperposition tilemap step =
    let
        stepCell =
            case step of
                PickTile cell _ ->
                    cell

                PlaceSubgridTile cell _ _ ->
                    cell

                KeepMatching _ toCell ->
                    toCell
    in
    Tilemap.tileAtAny tilemap stepCell
        |> Maybe.andThen
            (\tile ->
                case tile.kind of
                    Tile.Fixed _ ->
                        Nothing

                    Tile.Superposition options ->
                        List.Nonempty.fromList options
            )


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

        PlaceSubgridTile cell _ _ ->
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
                    attemptPlanLargeTilePlacement tilemap cell largeTile
                        |> Result.map (\steps -> ( steps, tilemap ))

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
                    Ok ( nextSteps, Tilemap.setTile cell tile tilemap )

        PlaceSubgridTile cell singleTile largeTileId ->
            attemptPlaceSubgridTile tilemap largeTileId cell singleTile
                |> Result.map
                    (\( neighborSteps, nextTilemap ) ->
                        ( neighborSteps
                        , nextTilemap
                        )
                    )

        KeepMatching from to ->
            Maybe.map2 Tuple.pair
                (Tilemap.tileAtAny tilemap from)
                (Tilemap.tileAtAny tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (updateSuperpositionOptions tilemap from to)
                |> Result.map (\nextTilemap -> ( [], nextTilemap ))


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
    (NeighborUpdateContext -> TileId -> Result error (List PropagationStep))
    -> (NeighborUpdateContext -> List TileId -> Result error (List PropagationStep))
    -> Cell
    -> Tilemap
    -> Result error (List PropagationStep)
applyToNeighbor onFixed onSuperposition cell tilemap =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    attemptFoldList
        (\dir steps ->
            case Cell.nextOrthogonalCell tilemapConfig dir cell of
                Just toCell ->
                    case Tilemap.tileAtAny tilemap toCell of
                        Just toTile ->
                            let
                                neighborUpdateContext =
                                    NeighborUpdateContext dir toTile toCell tilemap steps
                            in
                            case toTile.kind of
                                Fixed tileId ->
                                    onFixed neighborUpdateContext tileId

                                Superposition options ->
                                    onSuperposition neighborUpdateContext options

                        Nothing ->
                            Ok steps

                Nothing ->
                    Ok steps
        )
        []
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


{-| Tries to build a list of PlaceSubgridTile steps to build the a large tile subgrid
-}
attemptPlanLargeTilePlacement : Tilemap -> Cell -> LargeTile -> Result PropagationFailure (List PropagationStep)
attemptPlanLargeTilePlacement tilemap anchorCell tile =
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
                                    Ok (PlaceSubgridTile cell singleTile tile.id)

                                Nothing ->
                                    Err "Cannot translate cell to global coordinates"
                        )
            )
        |> Result.mapError (\_ -> InvalidBigTilePlacement anchorCell tile.id)


{-| Try to place a subgrid tile (of a large tile), generating more propagation steps
-}
attemptPlaceSubgridTile : Tilemap -> TileId -> Cell -> SingleTile -> Result PropagationFailure ( List PropagationStep, Tilemap )
attemptPlaceSubgridTile tilemap largeTileId cell currentTile =
    tilemap
        |> attemptTileNeighborUpdate currentTile cell
        |> Result.map
            (\neighborSteps ->
                let
                    -- TODO: ignoring actions
                    ( tile, _ ) =
                        Tile.new currentTile.id Tile.BuildInstantly
                in
                ( neighborSteps, Tilemap.setTile cell tile tilemap )
            )
        |> Result.mapError (\_ -> InvalidBigTilePlacement cell largeTileId)


{-| Try to build a list of propagation steps for the tile's neighbor
-}
attemptTileNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result String (List PropagationStep)
attemptTileNeighborUpdate currentTile originCell tilemap =
    applyToNeighbor
        (\nuCtx neighborTileId ->
            let
                originSocket =
                    socketByDirection currentTile.sockets nuCtx.dir
            in
            if canDock nuCtx.tilemap (oppositeOrthogonalDirection nuCtx.dir) originSocket neighborTileId then
                -- Tiles can dock, no tilemap update needed = skip
                Ok nuCtx.steps

            else
                Err "Can't dock fixed neighbor"
        )
        (\nuCtx _ ->
            Ok (KeepMatching originCell nuCtx.neighborCell :: nuCtx.steps)
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
        , previousSteps : List String
        }
propagationContextDebug (Model modelDetails) =
    { position =
        [ Maybe.map Cell.toString modelDetails.currentCell |> Maybe.withDefault "-"
        , Maybe.map Cell.toString modelDetails.targetCell |> Maybe.withDefault "-"
        , Maybe.map orthogonalDirectionToString modelDetails.propagationDirection |> Maybe.withDefault "-"
        ]
    , openSteps = List.map stepDebug modelDetails.openSteps
    , previousSteps = modelDetails.previousSteps |> Stack.toList |> List.map previousStepDebug
    }


stepDebug : PropagationStep -> String
stepDebug step =
    case step of
        PickTile cell tileConfig ->
            String.join " "
                [ "PickTile        "
                , Cell.toString cell
                , TileConfig.toString tileConfig
                ]

        PlaceSubgridTile cell singleTile largeTileId ->
            String.join " "
                [ "PlaceSubgridTile"
                , Cell.toString cell
                , "tile ID:"
                , String.fromInt singleTile.id
                , "large tile ID: "
                , String.fromInt largeTileId
                ]

        KeepMatching fromCell toCell ->
            "KeepMatching     " ++ Cell.toString fromCell ++ " -> " ++ Cell.toString toCell


previousStepDebug : ( PropagationStep, Nonempty TileId ) -> String
previousStepDebug ( step, superpositionOptions ) =
    let
        optionsDebug =
            superpositionOptions
                |> List.Nonempty.toList
                |> List.map String.fromInt
                |> String.join ", "
    in
    stepDebug step ++ " / " ++ optionsDebug
