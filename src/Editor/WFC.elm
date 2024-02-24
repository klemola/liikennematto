module Editor.WFC exposing
    ( Model
    , contextDebug
    , failed
    , init
    , solve
    , stateDebug
    , step
    , stepN
    , stopped
    , toCurrentCell
    , toTilemap
    )

import Array
import Common exposing (attemptFoldList, attemptMapList)
import Data.TileSet exposing (pairingsForSocket, tileById, tileIds)
import List.Nonempty exposing (Nonempty)
import Model.Cell as Cell exposing (Cell)
import Model.Geometry
    exposing
        ( OrthogonalDirection
        , oppositeOrthogonalDirection
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
    , openSteps : List Step
    , previousSteps : Stack ( Step, Nonempty TileId )
    , state : WFCState
    , seed : Seed
    }


type Step
    = Collapse Cell TileConfig
    | CollapseSubgridCell Cell SingleTile TileId
    | PropagateConstraints Cell Cell


type WFCState
    = Solving
    | Done
    | Recovering WFCFailure
    | Failed WFCFailure


type WFCFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidBigTilePlacement Cell TileId
    | InvalidDirection
    | NoCandidates
    | TileNotFound
    | BacktrackFailed


init : TilemapConfig -> Random.Seed -> Model
init tilemapConfig initialSeed =
    Model
        { tilemap = Tilemap.empty tilemapConfig
        , currentCell = Nothing
        , targetCell = Nothing
        , openSteps = []
        , previousSteps = Stack.empty
        , state = Solving
        , seed = initialSeed
        }


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position
-}
solve : TilemapConfig -> Random.Seed -> Model
solve tilemapConfig initialSeed =
    let
        nextModel =
            step <| init tilemapConfig initialSeed
    in
    solve_ nextModel


solve_ : Model -> Model
solve_ model =
    if stopped model then
        model

    else
        solve_ (step model)


step : Model -> Model
step model =
    let
        ((Model modelDetails) as afterStep) =
            step_ model
    in
    case modelDetails.state of
        Recovering wfcFailure ->
            case wfcFailure of
                NoPotentialMatch ->
                    -- Aknowledge failure, can resume
                    Model { modelDetails | state = Solving }

                _ ->
                    case backtrack modelDetails.previousSteps modelDetails.tilemap of
                        Ok tilemapAfterBacktrack ->
                            Model
                                { modelDetails
                                    | state = Solving
                                    , openSteps = []
                                    , tilemap = tilemapAfterBacktrack
                                }

                        Err _ ->
                            -- Backtracking failed, no way to continue
                            Model { modelDetails | state = Failed wfcFailure }

        _ ->
            afterStep


stepN : Int -> Model -> Model
stepN nTimes model =
    if nTimes == 0 then
        model

    else
        let
            stepResult =
                step model
        in
        if failed stepResult then
            stepResult

        else
            stepN (nTimes - 1) stepResult


{-| Execute a single step. This can mean picking the next random tile
or propagating constraints resulting from the last placement of a tile
-}
step_ : Model -> Model
step_ (Model ({ openSteps, previousSteps, tilemap } as modelDetails)) =
    case openSteps of
        currentStep :: otherSteps ->
            let
                ( currentCell, targetCell ) =
                    case currentStep of
                        Collapse cell _ ->
                            ( Just cell, Nothing )

                        CollapseSubgridCell cell _ _ ->
                            ( Just cell, Nothing )

                        PropagateConstraints cellA cellB ->
                            ( Just cellA, Just cellB )

                withPosition =
                    { modelDetails
                        | currentCell = currentCell
                        , targetCell = targetCell
                    }
            in
            case processStep tilemap currentStep of
                Ok ( additionalSteps, nextTilemap ) ->
                    Model
                        { withPosition
                            | tilemap = nextTilemap
                            , state = Solving
                            , openSteps = otherSteps ++ additionalSteps
                            , previousSteps =
                                case stepSuperposition tilemap currentStep of
                                    Just superpositionOptions ->
                                        Stack.push ( currentStep, superpositionOptions ) previousSteps

                                    Nothing ->
                                        previousSteps
                        }

                Err wfcFailure ->
                    Model
                        { withPosition
                            | state = Recovering wfcFailure
                            , openSteps = otherSteps
                        }

        [] ->
            let
                nextModelDetails =
                    if not (solved modelDetails) then
                        let
                            withPick =
                                pickRandom modelDetails
                        in
                        { withPick | state = Solving }

                    else
                        { modelDetails
                            | state = Done
                            , currentCell = Nothing
                            , targetCell = Nothing
                        }
            in
            Model nextModelDetails



--
-- Steps and propagation
--


processStep : Tilemap -> Step -> Result WFCFailure ( List Step, Tilemap )
processStep tilemap currentStep =
    case currentStep of
        Collapse cell tileConfig ->
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
                                |> Maybe.map (PropagateConstraints cell)

                        nextSteps =
                            List.filterMap stepInDirection orthogonalDirections

                        -- TODO: ignoring actions
                        ( tile, _ ) =
                            Tile.new singleTile.id Tile.BuildInstantly
                    in
                    Ok ( nextSteps, Tilemap.setTile cell tile tilemap )

        CollapseSubgridCell cell singleTile largeTileId ->
            attemptPlaceSubgridTile tilemap largeTileId cell singleTile
                |> Result.map
                    (\( neighborSteps, nextTilemap ) ->
                        ( neighborSteps
                        , nextTilemap
                        )
                    )

        PropagateConstraints from to ->
            Maybe.map2 Tuple.pair
                (Tilemap.tileAtAny tilemap from)
                (Tilemap.tileAtAny tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (updateSuperpositionOptions tilemap from to)
                |> Result.map (\nextTilemap -> ( [], nextTilemap ))


updateSuperpositionOptions : Tilemap -> Cell -> Cell -> ( Tile, Tile ) -> Result WFCFailure Tilemap
updateSuperpositionOptions tilemap from to tilePair =
    Cell.orthogonalDirection from to
        |> Result.fromMaybe InvalidDirection
        |> Result.andThen
            (superpositionOptionsForTile tilePair)
        |> Result.map
            (\superpositionOptions ->
                Tilemap.setSuperpositionOptions to superpositionOptions tilemap
            )


superpositionOptionsForTile :
    ( Tile, Tile )
    -> OrthogonalDirection
    -> Result WFCFailure (List Int)
superpositionOptionsForTile ( originTile, targetTile ) dir =
    case ( originTile.kind, targetTile.kind ) of
        ( Fixed originTileId, Superposition options ) ->
            let
                revisedOptions =
                    matchingSuperpositionOptions dir originTileId options
            in
            if List.isEmpty revisedOptions then
                Err NoSuperpositionOptions

            else
                Ok revisedOptions

        _ ->
            Err NoPotentialMatch


matchingSuperpositionOptions : OrthogonalDirection -> TileId -> List TileId -> List TileId
matchingSuperpositionOptions dir originTileId options =
    let
        originTileConfig =
            tileById originTileId

        originSocket =
            socketByDirectionWithConfig originTileConfig dir
    in
    List.filter
        (canDock
            (oppositeOrthogonalDirection dir)
            originSocket
        )
        options


canDock : OrthogonalDirection -> Socket -> Int -> Bool
canDock dockDir dockSocket dockTileId =
    let
        dockTileConfig =
            tileById dockTileId

        matchSocket =
            socketByDirectionWithConfig dockTileConfig dockDir

        pairings =
            pairingsForSocket dockSocket
    in
    List.member matchSocket pairings


{-| Returns true if all positions in the grid have a tile assigned
-}
solved : ModelDetails -> Bool
solved { tilemap, openSteps } =
    List.isEmpty openSteps && Tilemap.all solvedPredicate tilemap


solvedPredicate : Tile -> Bool
solvedPredicate tile =
    case tile.kind of
        Superposition _ ->
            False

        Fixed _ ->
            True


stepSuperposition : Tilemap -> Step -> Maybe (Nonempty TileId)
stepSuperposition tilemap theStep =
    Tilemap.tileAtAny tilemap (stepCell theStep)
        |> Maybe.andThen
            (\tile ->
                case tile.kind of
                    Tile.Fixed _ ->
                        Nothing

                    Tile.Superposition options ->
                        List.Nonempty.fromList options
            )


stepCell : Step -> Cell
stepCell theStep =
    case theStep of
        Collapse cell _ ->
            cell

        CollapseSubgridCell cell _ _ ->
            cell

        PropagateConstraints _ toCell ->
            toCell


stepTileId : Step -> Maybe TileId
stepTileId theStep =
    case theStep of
        Collapse _ tileConfig ->
            Just <| TileConfig.tileConfigId tileConfig

        CollapseSubgridCell _ _ largeTileId ->
            Just largeTileId

        PropagateConstraints _ _ ->
            Nothing



--
-- Backtracking
--


backtrack : Stack ( Step, Nonempty TileId ) -> Tilemap -> Result WFCFailure Tilemap
backtrack previousSteps tilemap =
    let
        ( stepCtx, previousSteps_ ) =
            Stack.pop previousSteps
    in
    case stepCtx of
        Nothing ->
            Err BacktrackFailed

        Just ( theStep, previousSuperposition ) ->
            let
                ( remainingSuperposition, reverted ) =
                    revertStep theStep previousSuperposition tilemap

                backtrackedEnough =
                    case theStep of
                        Collapse _ _ ->
                            not <| List.isEmpty remainingSuperposition

                        _ ->
                            False
            in
            if backtrackedEnough then
                Ok reverted

            else
                backtrack previousSteps_ reverted


revertStep : Step -> Nonempty TileId -> Tilemap -> ( List TileId, Tilemap )
revertStep theStep previousSuperposition tilemap =
    let
        targetCell =
            stepCell theStep

        maybeTileId =
            stepTileId theStep

        psList =
            List.Nonempty.toList previousSuperposition

        nextSuperpositionOptions =
            case maybeTileId of
                Just tileId ->
                    List.filter (\superpositionOption -> superpositionOption /= tileId) psList

                Nothing ->
                    psList
    in
    ( nextSuperpositionOptions
    , Tilemap.setSuperpositionOptions targetCell nextSuperpositionOptions tilemap
    )



--
-- Random pick for collapse
--


type alias Candidate =
    { cell : Cell
    , options : Nonempty TileId
    }


pickRandom : ModelDetails -> ModelDetails
pickRandom ({ openSteps, tilemap, seed } as modelDetails) =
    case
        List.Nonempty.fromList (nextCandidates tilemap)
    of
        Nothing ->
            { modelDetails | state = Recovering NoCandidates }

        Just candidates ->
            let
                ( randomCandidate, seedAfterCandidateGen ) =
                    Random.step (List.Nonempty.sample candidates) seed

                randomOptionGen =
                    toWeightedOptions randomCandidate.options

                ( randomOption, nextSeed ) =
                    Random.step randomOptionGen seedAfterCandidateGen

                collapseStep =
                    [ Collapse randomCandidate.cell randomOption ]
            in
            { modelDetails
                | openSteps = collapseStep ++ openSteps
                , currentCell = Nothing
                , targetCell = Nothing
                , seed = nextSeed
            }


toWeightedOptions : Nonempty TileId -> Random.Generator TileConfig
toWeightedOptions options =
    let
        withWeights =
            List.Nonempty.map
                (\option ->
                    let
                        tileConfig =
                            tileById option
                    in
                    ( 1.0 - TileConfig.complexity tileConfig, tileConfig )
                )
                options
    in
    Random.weighted (List.Nonempty.head withWeights) (List.Nonempty.toList withWeights)


nextCandidates : Tilemap -> List Candidate
nextCandidates tilemap =
    let
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
        |> Tilemap.fold toCandidate ( [], List.length tileIds + 1 )
        |> Tuple.first



--
-- Extra constraints
--


{-| Tries to build a list of PlaceSubgridTile steps to build the a large tile subgrid
-}
attemptPlanLargeTilePlacement : Tilemap -> Cell -> LargeTile -> Result WFCFailure (List Step)
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
                                    Ok (CollapseSubgridCell cell singleTile tile.id)

                                Nothing ->
                                    Err "Cannot translate cell to global coordinates"
                        )
            )
        |> Result.mapError (\_ -> InvalidBigTilePlacement anchorCell tile.id)


{-| Try to place a subgrid tile (of a large tile), generating more steps
-}
attemptPlaceSubgridTile : Tilemap -> TileId -> Cell -> SingleTile -> Result WFCFailure ( List Step, Tilemap )
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


{-| Try to build a list of steps for the tile's neighbor
-}
attemptTileNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result String (List Step)
attemptTileNeighborUpdate currentTile originCell tilemap =
    applyToNeighbor
        (\nuCtx neighborTileId ->
            let
                originSocket =
                    socketByDirection currentTile.sockets nuCtx.dir
            in
            if canDock (oppositeOrthogonalDirection nuCtx.dir) originSocket neighborTileId then
                -- Tiles can dock, no tilemap update needed = skip
                Ok nuCtx.steps

            else
                Err "Can't dock fixed neighbor"
        )
        (\nuCtx _ ->
            Ok (PropagateConstraints originCell nuCtx.neighborCell :: nuCtx.steps)
        )
        originCell
        tilemap



--
-- Helpers
--


type alias NeighborUpdateContext =
    { dir : OrthogonalDirection
    , neighborTile : Tile
    , neighborCell : Cell
    , tilemap : Tilemap
    , steps : List Step
    }


applyToNeighbor :
    (NeighborUpdateContext -> TileId -> Result error (List Step))
    -> (NeighborUpdateContext -> List TileId -> Result error (List Step))
    -> Cell
    -> Tilemap
    -> Result error (List Step)
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



--
-- Interop, queries and debug
--


stopped : Model -> Bool
stopped (Model modelDetails) =
    modelDetails.state /= Solving


failed : Model -> Bool
failed (Model modelDetails) =
    case modelDetails.state of
        Failed _ ->
            True

        _ ->
            False


toCurrentCell : Model -> Maybe Cell
toCurrentCell (Model modelDetails) =
    modelDetails.currentCell


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


stateDebug : Model -> String
stateDebug (Model modelDetails) =
    case modelDetails.state of
        Solving ->
            "solving"

        Recovering wfcFailure ->
            String.join " "
                [ "recovering:"
                , wfcFailureToString wfcFailure
                ]

        Failed wfcFailure ->
            String.join " "
                [ "failed:"
                , wfcFailureToString wfcFailure
                ]

        Done ->
            "done"


wfcFailureToString : WFCFailure -> String
wfcFailureToString wfcFailure =
    case wfcFailure of
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

        BacktrackFailed ->
            "Backtrack failed"


contextDebug :
    Model
    ->
        { position : List String
        , openSteps : List String
        , previousSteps : List String
        }
contextDebug (Model modelDetails) =
    { position =
        [ Maybe.map Cell.toString modelDetails.currentCell |> Maybe.withDefault "-"
        , Maybe.map Cell.toString modelDetails.targetCell |> Maybe.withDefault "-"
        ]
    , openSteps = List.map stepDebug modelDetails.openSteps
    , previousSteps = modelDetails.previousSteps |> Stack.toList |> List.map previousStepDebug
    }


stepDebug : Step -> String
stepDebug theStep =
    case theStep of
        Collapse cell tileConfig ->
            String.join " "
                [ "Collapse            "
                , Cell.toString cell
                , TileConfig.toString tileConfig
                ]

        CollapseSubgridCell cell singleTile largeTileId ->
            String.join " "
                [ "CollapseSubgridCell "
                , Cell.toString cell
                , "tile ID:"
                , String.fromInt singleTile.id
                , "large tile ID: "
                , String.fromInt largeTileId
                ]

        PropagateConstraints fromCell toCell ->
            "PropagateConstraints " ++ Cell.toString fromCell ++ " -> " ++ Cell.toString toCell


previousStepDebug : ( Step, Nonempty TileId ) -> String
previousStepDebug ( theStep, superpositionOptions ) =
    let
        optionsDebug =
            superpositionOptions
                |> List.Nonempty.toList
                |> List.map String.fromInt
                |> String.join ", "
    in
    stepDebug theStep ++ " / " ++ optionsDebug
