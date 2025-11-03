module Tilemap.WFC exposing
    ( Model
    , StepEndCondition(..)
    , WFCFailure
    , WFCState(..)
    , checkLargeTileFit
    , collapse
    , collapsedTiles
    , contextDebug
    , currentSeed
    , currentState
    , debug_collapseWithId
    , flushPendingActions
    , fromTilemap
    , largeTileSubgrid
    , log
    , propagateConstraints
    , solve
    , stateDebug
    , step
    , stepN
    , stopped
    , toCurrentCell
    , toTilemap
    , withTileInventory
    , withTilemapUpdate
    )

import Array
import Common exposing (attemptFoldList, attemptMapList)
import Data.TileSet as TileSet
    exposing
        ( defaultSocket
        , pairingsForSocket
        , tileById
        )
import Dict
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import Lib.SeedState as SeedState exposing (SeedState)
import List.Nonempty exposing (Nonempty)
import Random
import Random.Extra
import Stack exposing (Stack)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , addTileFromWfc
        , foldTiles
        , getTilemapConfig
        , setSuperpositionOptions
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig
    exposing
        ( LargeTile
        , SingleTile
        , Socket
        , SubgridTileProperties
        , TileConfig
        , TileId
        , socketByDirection
        , socketByDirectionWithConfig
        )
import Tilemap.TileInventory as TileInventory exposing (TileInventory)


type Model
    = Model ModelDetails


type alias ModelDetails =
    { seed : SeedState
    , state : WFCState
    , tilemap : Tilemap
    , currentCell : Maybe Cell
    , targetCell : Maybe Cell
    , openSteps : List Step
    , previousSteps : PreviousSteps
    , backtrackCount : Int
    , log : List String
    , pendingActions : List Tile.Action
    , tileInventory : TileInventory Int
    }


type Step
    = Collapse Cell TileConfig
    | CollapseSubgridCell Cell SubgridTileProperties
    | PropagateConstraints Cell Cell


type WFCState
    = Solving
    | Done
    | Recovering WFCFailure
    | Failed WFCFailure


type WFCFailure
    = NoSuperpositionOptions
    | InvalidBigTilePlacement Cell TileId String
    | InvalidDirection
    | TileNotFound
    | TileUnavailable TileId
    | BacktrackFailed


type alias PreviousSteps =
    Stack ( Step, Nonempty TileId )


maxBacktrackCount : Int
maxBacktrackCount =
    100



--
-- Init
--


fromTilemap : Tilemap -> SeedState -> Model
fromTilemap tilemap initialSeedState =
    Model
        { seed = initialSeedState
        , state = Solving
        , tilemap = tilemap
        , currentCell = Nothing
        , targetCell = Nothing
        , openSteps = []
        , previousSteps = Stack.empty
        , backtrackCount = 0
        , log = []
        , pendingActions = []
        , tileInventory = Dict.empty
        }


withTileInventory : TileInventory Int -> Model -> Model
withTileInventory tileInventory (Model modelContents) =
    Model { modelContents | tileInventory = tileInventory }



--
-- Automated solving
--


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position
-}
solve : Model -> Model
solve initialModel =
    let
        nextModel =
            step StopAtSolved initialModel
    in
    solve_ nextModel


solve_ : Model -> Model
solve_ model =
    if stopped model then
        model

    else
        solve_ (step StopAtSolved model)


type StepEndCondition
    = StopAtSolved
    | StopAtEmptySteps


step : StepEndCondition -> Model -> Model
step endCondition model =
    let
        ((Model steppedModel) as afterStep) =
            processOpenSteps endCondition model
    in
    case steppedModel.state of
        Recovering _ ->
            case backtrack steppedModel.previousSteps steppedModel.tileInventory steppedModel.tilemap of
                Ok ( prunedPreviousSteps, updatedTileInventory, tilemapAfterBacktrack ) ->
                    if steppedModel.backtrackCount + 1 > maxBacktrackCount then
                        let
                            nextState =
                                Failed BacktrackFailed
                        in
                        Model
                            { steppedModel
                                | state = nextState
                                , log = ("<" ++ stateDebugInternal nextState) :: steppedModel.log
                            }

                    else
                        let
                            nextBacktrackCount =
                                steppedModel.backtrackCount + 1
                        in
                        Model
                            { steppedModel
                                | state = Solving
                                , openSteps = []
                                , previousSteps = prunedPreviousSteps
                                , backtrackCount = nextBacktrackCount
                                , log = ("<Backtrack successful, count: " ++ String.fromInt nextBacktrackCount) :: steppedModel.log
                                , tilemap = tilemapAfterBacktrack
                                , tileInventory = updatedTileInventory
                            }

                Err failure ->
                    -- Backtracking failed, no way to continue
                    Model
                        { steppedModel
                            | state = Failed failure
                            , log = "!Backtrack failed" :: steppedModel.log
                        }

        _ ->
            afterStep


stepN : StepEndCondition -> Int -> Model -> Model
stepN endCondition nTimes model =
    if nTimes == 0 then
        flushOpenSteps model

    else
        let
            ((Model modelContents) as stepResult) =
                step endCondition model
        in
        case modelContents.state of
            Failed _ ->
                stepResult

            Done ->
                stepResult

            _ ->
                stepN endCondition (nTimes - 1) stepResult



--
-- Driven WFC
--


collapse : Cell -> Model -> ( Model, Maybe TileConfig )
collapse cell model =
    let
        (Model modelDetails) =
            step StopAtEmptySteps model
    in
    case tileByCell modelDetails.tilemap cell |> Maybe.map .kind of
        Just (Superposition options) ->
            let
                ( randomTileId, nextSeedState ) =
                    SeedState.step (Random.Extra.sample options) modelDetails.seed
            in
            case randomTileId |> Maybe.map tileById of
                Just tileConfig ->
                    let
                        withCollapsedCell =
                            Model
                                { modelDetails
                                    | openSteps = Collapse cell tileConfig :: modelDetails.openSteps
                                    , seed = nextSeedState
                                    , state = Solving
                                }
                    in
                    ( flushOpenSteps withCollapsedCell
                    , Just tileConfig
                    )

                _ ->
                    ( Model { modelDetails | seed = nextSeedState }, Nothing )

        _ ->
            ( model, Nothing )


propagateConstraints : Cell -> Model -> Model
propagateConstraints cell (Model modelDetails) =
    let
        newSteps =
            propagateConstraintsSteps modelDetails.tilemap cell

        withSteps =
            Model
                { modelDetails
                    | openSteps = modelDetails.openSteps ++ newSteps
                }
    in
    flushOpenSteps withSteps


flushOpenSteps : Model -> Model
flushOpenSteps ((Model modelContents) as model) =
    if List.isEmpty modelContents.openSteps then
        model

    else
        flushOpenSteps (step StopAtEmptySteps model)


withTilemapUpdate : (Tilemap -> Tilemap) -> Model -> Model
withTilemapUpdate updateTilemap (Model modelDetails) =
    Model { modelDetails | tilemap = updateTilemap modelDetails.tilemap }



--
-- Steps and propagation
--


type alias ProcessStepResult =
    ( List Step, List Tile.Action, Tilemap )


processOpenSteps : StepEndCondition -> Model -> Model
processOpenSteps endCondition (Model ({ openSteps, previousSteps, tilemap } as modelDetails)) =
    case openSteps of
        currentStep :: otherSteps ->
            let
                ( currentCell, targetCell ) =
                    case currentStep of
                        Collapse cell _ ->
                            ( Just cell, Nothing )

                        CollapseSubgridCell cell _ ->
                            ( Just cell, Nothing )

                        PropagateConstraints cellA cellB ->
                            ( Just cellA, Just cellB )

                withPosition =
                    { modelDetails
                        | currentCell = currentCell
                        , targetCell = targetCell
                    }
            in
            case processStep tilemap modelDetails.tileInventory currentStep of
                Ok ( additionalSteps, tileActions, nextTilemap ) ->
                    let
                        nextTileInventory =
                            case currentStep of
                                CollapseSubgridCell _ props ->
                                    -- Check if the next step continues to build the same large tile
                                    if (List.head otherSteps |> Maybe.andThen stepTileId) == Just props.parentTileId then
                                        modelDetails.tileInventory

                                    else
                                        TileInventory.markAsUsed props.parentTileId modelDetails.tileInventory

                                _ ->
                                    modelDetails.tileInventory
                    in
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
                            , pendingActions = modelDetails.pendingActions ++ tileActions
                            , tileInventory = nextTileInventory
                            , log = ("|Step " ++ stepDebug currentStep) :: withPosition.log
                        }

                Err wfcFailure ->
                    let
                        nextOtherSteps =
                            case wfcFailure of
                                InvalidBigTilePlacement _ _ _ ->
                                    []

                                NoSuperpositionOptions ->
                                    []

                                _ ->
                                    otherSteps

                        nextState =
                            Recovering wfcFailure
                    in
                    Model
                        { withPosition
                            | state = nextState
                            , openSteps = nextOtherSteps
                            , log =
                                ("<" ++ stateDebugInternal nextState)
                                    :: ("!FAIL " ++ stepDebug currentStep)
                                    :: withPosition.log
                        }

        [] ->
            let
                nextModelDetails =
                    case endCondition of
                        StopAtEmptySteps ->
                            { modelDetails
                                | currentCell = Nothing
                                , targetCell = Nothing
                            }

                        StopAtSolved ->
                            pickRandom modelDetails
            in
            Model nextModelDetails


processStep : Tilemap -> TileInventory Int -> Step -> Result WFCFailure ProcessStepResult
processStep tilemap tileInventory currentStep =
    case currentStep of
        Collapse cell tileConfig ->
            let
                tileId =
                    TileConfig.tileConfigId tileConfig
            in
            if not (TileInventory.isAvailable tileId tileInventory) then
                Err (TileUnavailable tileId)

            else
                case tileConfig of
                    TileConfig.Large largeTile ->
                        largeTileSteps tilemap cell largeTile
                            |> Result.map (\steps -> ( steps, [], tilemap ))

                    TileConfig.Single _ ->
                        let
                            nextSteps =
                                propagateConstraintsSteps tilemap cell

                            ( nextTilemap, tileActions ) =
                                addTileFromWfc Nothing tileConfig cell tilemap
                        in
                        Ok ( nextSteps, tileActions, nextTilemap )

        CollapseSubgridCell cell properties ->
            attemptPlaceSubgridTile tilemap properties.parentTileId cell properties

        PropagateConstraints from to ->
            Maybe.map2 Tuple.pair
                (tileByCell tilemap from)
                (tileByCell tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (attemptPropagateConstraints tilemap from to)
                |> Result.map (\nextTilemap -> ( [], [], nextTilemap ))


propagateConstraintsSteps : Tilemap -> Cell -> List Step
propagateConstraintsSteps tilemap cell =
    let
        tilemapConfig =
            getTilemapConfig tilemap

        stepInDirection dir =
            Cell.nextOrthogonalCell tilemapConfig dir cell
                |> Maybe.map (PropagateConstraints cell)
    in
    List.filterMap stepInDirection OrthogonalDirection.all


attemptPropagateConstraints : Tilemap -> Cell -> Cell -> ( Tile, Tile ) -> Result WFCFailure Tilemap
attemptPropagateConstraints tilemap from to ( originTile, targetTile ) =
    Cell.orthogonalDirection from to
        |> Result.fromMaybe InvalidDirection
        |> Result.andThen
            (\dir ->
                case ( originTile.kind, targetTile.kind ) of
                    ( Fixed tileProperties, Superposition options ) ->
                        let
                            revisedOptions =
                                matchingSuperpositionOptions dir tileProperties.id options
                        in
                        if List.isEmpty revisedOptions then
                            Err NoSuperpositionOptions

                        else
                            Ok (setSuperpositionOptions to revisedOptions tilemap)

                    _ ->
                        Ok tilemap
            )


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
            (OrthogonalDirection.opposite dir)
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


stepSuperposition : Tilemap -> Step -> Maybe (Nonempty TileId)
stepSuperposition tilemap theStep =
    tileByCell tilemap (stepCell theStep)
        |> Maybe.andThen
            (\tile ->
                case tile.kind of
                    Tile.Superposition options ->
                        List.Nonempty.fromList options

                    _ ->
                        Nothing
            )


stepCell : Step -> Cell
stepCell theStep =
    case theStep of
        Collapse cell _ ->
            cell

        CollapseSubgridCell cell _ ->
            cell

        PropagateConstraints _ toCell ->
            toCell


stepTileId : Step -> Maybe TileId
stepTileId theStep =
    case theStep of
        Collapse _ tileConfig ->
            Just <| TileConfig.tileConfigId tileConfig

        CollapseSubgridCell _ properties ->
            Just properties.parentTileId

        PropagateConstraints _ _ ->
            Nothing



--
-- Backtracking
--


backtrack : PreviousSteps -> TileInventory Int -> Tilemap -> Result WFCFailure ( PreviousSteps, TileInventory Int, Tilemap )
backtrack previousSteps tileInventory tilemap =
    let
        ( stepCtx, remainingPreviousSteps ) =
            Stack.pop previousSteps
    in
    case stepCtx of
        Nothing ->
            -- No steps left, should be possible to continue
            Ok ( remainingPreviousSteps, tileInventory, tilemap )

        Just ( theStep, previousSuperposition ) ->
            let
                ( remainingSuperposition, updatedInventory, reverted ) =
                    revertStep theStep previousSuperposition tileInventory tilemap

                backtrackedEnough =
                    case theStep of
                        Collapse _ _ ->
                            List.length remainingSuperposition > 0

                        _ ->
                            False
            in
            if backtrackedEnough then
                Ok ( remainingPreviousSteps, updatedInventory, reverted )

            else
                backtrack remainingPreviousSteps updatedInventory reverted


revertStep : Step -> Nonempty TileId -> TileInventory Int -> Tilemap -> ( List TileId, TileInventory Int, Tilemap )
revertStep theStep previousSuperposition tileInventory tilemap =
    let
        targetCell =
            stepCell theStep

        psList =
            List.Nonempty.toList previousSuperposition

        ( nextSuperpositionOptions, updatedInventory ) =
            case stepTileId theStep of
                Just tileId ->
                    ( List.filter (\superpositionOption -> superpositionOption /= tileId) psList
                    , TileInventory.increaseCount tileId tileInventory
                    )

                Nothing ->
                    ( psList, tileInventory )
    in
    ( nextSuperpositionOptions
    , updatedInventory
    , setSuperpositionOptions targetCell nextSuperpositionOptions tilemap
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
            { modelDetails
                | state = Done
                , currentCell = Nothing
                , targetCell = Nothing
                , log = stateDebugInternal Done :: modelDetails.log
            }

        Just candidates ->
            let
                ( randomCandidate, seedAfterCandidateGen ) =
                    SeedState.step (List.Nonempty.sample candidates) seed

                randomOptionGen =
                    toWeightedOptions randomCandidate.options

                ( randomOption, nextSeedState ) =
                    SeedState.step randomOptionGen seedAfterCandidateGen

                collapseStep =
                    [ Collapse randomCandidate.cell randomOption ]

                logEntry =
                    List.Nonempty.foldl
                        (\c entry ->
                            String.join "\n.."
                                [ entry
                                , String.join " "
                                    (Cell.toString c.cell
                                        :: (c.options
                                                |> List.Nonempty.toList
                                                |> List.map String.fromInt
                                           )
                                    )
                                ]
                        )
                        (String.join " "
                            [ ".Pick random: "
                            , Cell.toString randomCandidate.cell
                            , String.fromInt (TileConfig.tileConfigId randomOption)
                            ]
                        )
                        candidates
            in
            { modelDetails
                | openSteps = collapseStep ++ openSteps
                , currentCell = Nothing
                , targetCell = Nothing
                , seed = nextSeedState
                , log = logEntry :: modelDetails.log
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
                    ( TileConfig.weight tileConfig, tileConfig )
                )
                options
    in
    Random.weighted (List.Nonempty.head withWeights) (List.Nonempty.toList withWeights)


nextCandidates : Tilemap -> List Candidate
nextCandidates tilemap =
    let
        toCandidate cell tile ( candidates, minEntropy ) =
            case tile.kind of
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
                                -- Same entropy
                                ( { cell = cell, options = nonemptyOptions } :: candidates
                                , currentEntropy
                                )

                        Nothing ->
                            ( candidates, minEntropy )

                _ ->
                    ( candidates, minEntropy )
    in
    tilemap
        |> foldTiles toCandidate ( [], TileSet.allTilesAmount + 1 )
        |> Tuple.first



--
-- Extra constraints
--


{-| Tries to build a list of steps to build a large tile
-}
largeTileSteps : Tilemap -> Cell -> LargeTile -> Result WFCFailure (List Step)
largeTileSteps tilemap anchorCell largeTile =
    largeTileSubgrid tilemap anchorCell largeTile
        |> Result.map
            (List.map (\( cell, props ) -> CollapseSubgridCell cell props))
        |> Result.mapError
            (\err ->
                InvalidBigTilePlacement anchorCell largeTile.id err
            )


{-| Does a large tile fit into the tilemap?
Checks if the large tile subgrid cells and the anchor cell are compatible
-}
checkLargeTileFit : Tilemap -> Cell -> LargeTile -> Maybe LargeTile
checkLargeTileFit tilemap anchorCell largeTile =
    largeTileSubgrid tilemap anchorCell largeTile
        |> Result.andThen
            (List.foldl
                (\( subgridCell, subgridProps ) acc ->
                    case acc of
                        Ok nextTilemap ->
                            checkSubgridTile nextTilemap subgridCell subgridProps

                        Err _ ->
                            acc
                )
                (Ok tilemap)
            )
        |> Result.toMaybe
        |> Maybe.map (\_ -> largeTile)


{-| Tries to build a list of cells and subgrid tile data for a large tile
-}
largeTileSubgrid : Tilemap -> Cell -> LargeTile -> Result String (List ( Cell, SubgridTileProperties ))
largeTileSubgrid tilemap anchorCell tile =
    let
        tilemapConstraints =
            getTilemapConfig tilemap

        topLeftCornerCell =
            Tile.largeTileTopLeftCell tilemapConstraints anchorCell tile.anchorIndex tile
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
                                subgridDimensions =
                                    { horizontalCellsAmount = tile.width
                                    , verticalCellsAmount = tile.height
                                    }

                                cellInGlobalCoordinates =
                                    subgridIdx
                                        -- Use of unsafe function: we know that here that the Cell can be constructed
                                        |> Cell.fromArray1DIndexUnsafe subgridDimensions
                                        |> Cell.placeIn tilemapConstraints origin
                            in
                            case cellInGlobalCoordinates of
                                Just cell ->
                                    case tileByCell tilemap cell |> Maybe.map .kind of
                                        Just (Superposition _) ->
                                            Ok
                                                ( cell
                                                , { parentTileId = tile.id
                                                  , singleTile = singleTile
                                                  , index = subgridIdx
                                                  }
                                                )

                                        Just (Fixed _) ->
                                            Err "Subgrid cell is fixed"

                                        _ ->
                                            Err "Subgrid cell is not available (may be uninitialized)"

                                Nothing ->
                                    Err "Cannot translate cell to global coordinates"
                        )
            )


{-| Try to place a subgrid tile (of a large tile), generating more steps
-}
attemptPlaceSubgridTile :
    Tilemap
    -> TileId
    -> Cell
    -> SubgridTileProperties
    -> Result WFCFailure ( List Step, List Tile.Action, Tilemap )
attemptPlaceSubgridTile tilemap largeTileId subgridCell subgridTileProperties =
    tilemap
        |> attemptTileNeighborUpdate subgridTileProperties.singleTile subgridCell
        |> Result.map
            (\neighborSteps ->
                let
                    parentTileProperties =
                        ( subgridTileProperties.parentTileId, subgridTileProperties.index )

                    ( nextTilemap, tileActions ) =
                        addTileFromWfc
                            (Just parentTileProperties)
                            (TileConfig.Single subgridTileProperties.singleTile)
                            subgridCell
                            tilemap
                in
                ( neighborSteps, tileActions, nextTilemap )
            )
        |> Result.mapError
            (\err ->
                InvalidBigTilePlacement subgridCell largeTileId err
            )


{-| Same as attemptPlaceSubgridTile, except the subgrid tile fit is only verified
-}
checkSubgridTile : Tilemap -> Cell -> SubgridTileProperties -> Result String Tilemap
checkSubgridTile tilemap subgridCell subgridTileProperties =
    tilemap
        |> attemptTileNeighborUpdate subgridTileProperties.singleTile subgridCell
        |> Result.map
            (\_ ->
                let
                    parentTileProperties =
                        ( subgridTileProperties.parentTileId, subgridTileProperties.index )

                    ( nextTilemap, _ ) =
                        addTileFromWfc
                            (Just parentTileProperties)
                            (TileConfig.Single subgridTileProperties.singleTile)
                            subgridCell
                            tilemap
                in
                nextTilemap
            )


{-| Try to build a list of steps for the tile's neighbor
-}
attemptTileNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result String (List Step)
attemptTileNeighborUpdate currentTile originCell tilemap =
    applyToNeighbor
        (\neighborTile nuCtx ->
            case neighborTile.kind of
                Fixed properties ->
                    if canDock (OrthogonalDirection.opposite nuCtx.dir) nuCtx.neighborSocket properties.id then
                        -- Tiles can dock, no tilemap update needed = skip
                        Ok nuCtx.steps

                    else
                        Err "Can't dock fixed neighbor"

                Superposition _ ->
                    if nuCtx.neighborSocket == defaultSocket then
                        Ok (PropagateConstraints originCell nuCtx.neighborCell :: nuCtx.steps)

                    else
                        -- Don't try to propagate constraints for edges that will match another subgrid tile or the entry point
                        Ok nuCtx.steps

                Unintialized ->
                    -- Allow docking on unitialized neighbor like it was the edge of the map
                    if nuCtx.neighborSocket == defaultSocket then
                        Ok nuCtx.steps

                    else
                        Err "Can't dock uninitialized neighbor"

                Buffer ->
                    Ok nuCtx.steps
        )
        originCell
        currentTile
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
    , neighborSocket : Socket
    }


applyToNeighbor :
    (Tile -> NeighborUpdateContext -> Result error (List Step))
    -> Cell
    -> SingleTile
    -> Tilemap
    -> Result error (List Step)
applyToNeighbor neighborUpdateFn cell currentTile tilemap =
    let
        tilemapConfig =
            getTilemapConfig tilemap
    in
    attemptFoldList
        (\dir steps ->
            case Cell.nextOrthogonalCell tilemapConfig dir cell of
                Just toCell ->
                    case tileByCell tilemap toCell of
                        Just toTile ->
                            let
                                neighborUpdateContext =
                                    NeighborUpdateContext dir toTile toCell tilemap steps (socketByDirection currentTile.sockets dir)
                            in
                            neighborUpdateFn toTile neighborUpdateContext

                        Nothing ->
                            Ok steps

                Nothing ->
                    Ok steps
        )
        []
        OrthogonalDirection.all



--
-- Interop, queries and debug
--


currentState : Model -> WFCState
currentState (Model modelDetails) =
    modelDetails.state


stopped : Model -> Bool
stopped (Model modelDetails) =
    modelDetails.state /= Solving


flushPendingActions : Model -> ( Model, List Tile.Action )
flushPendingActions ((Model modelDetails) as model) =
    if modelDetails.state == Done then
        ( Model { modelDetails | pendingActions = [] }
        , modelDetails.pendingActions
        )

    else
        ( model, [] )


toCurrentCell : Model -> Maybe Cell
toCurrentCell (Model modelDetails) =
    modelDetails.currentCell


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


log : Model -> List String
log (Model modelDetails) =
    modelDetails.log


currentSeed : Model -> SeedState
currentSeed (Model modelDetails) =
    modelDetails.seed


collapsedTiles : Model -> List ( Cell, TileId )
collapsedTiles (Model modelDetails) =
    let
        collect remaining acc =
            case Stack.pop remaining of
                ( Nothing, _ ) ->
                    acc

                ( Just previousStep, nextRemaining ) ->
                    let
                        ( theStep, _ ) =
                            previousStep

                        nextAcc =
                            case theStep of
                                Collapse cell tileConfig ->
                                    ( cell, TileConfig.tileConfigId tileConfig ) :: acc

                                _ ->
                                    acc
                    in
                    collect nextRemaining nextAcc
    in
    collect modelDetails.previousSteps []


stateDebug : Model -> String
stateDebug (Model modelDetails) =
    stateDebugInternal modelDetails.state


stateDebugInternal : WFCState -> String
stateDebugInternal state =
    case state of
        Solving ->
            "Solving"

        Recovering wfcFailure ->
            String.join " "
                [ "Recovering:"
                , wfcFailureToString wfcFailure
                ]

        Failed wfcFailure ->
            String.join " "
                [ "Failed:"
                , wfcFailureToString wfcFailure
                ]

        Done ->
            "Done"


wfcFailureToString : WFCFailure -> String
wfcFailureToString wfcFailure =
    case wfcFailure of
        NoSuperpositionOptions ->
            "No superposition opts"

        InvalidBigTilePlacement cell tileId reason ->
            String.join
                " "
                [ "Invalid big tile placement"
                , Cell.toString cell
                , String.fromInt tileId
                , "Reason:"
                , reason
                ]

        InvalidDirection ->
            "Invalid direction (cell to cell)"

        TileNotFound ->
            "Tile not found"

        TileUnavailable tileId ->
            "Tile unavailable: " ++ String.fromInt tileId

        BacktrackFailed ->
            "Backtrack failed"


contextDebug :
    Model
    ->
        { position : List String
        , openSteps : List String
        , previousSteps : List String
        , backtrackCount : Int
        }
contextDebug (Model modelDetails) =
    { position =
        [ Maybe.map Cell.toString modelDetails.currentCell |> Maybe.withDefault "-"
        , Maybe.map Cell.toString modelDetails.targetCell |> Maybe.withDefault "-"
        ]
    , openSteps = List.map stepDebug modelDetails.openSteps
    , previousSteps =
        modelDetails.previousSteps
            |> Stack.toList
            |> List.map previousStepDebug
    , backtrackCount = modelDetails.backtrackCount
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

        CollapseSubgridCell cell properties ->
            String.join " "
                [ "CollapseSubgridCell"
                , Cell.toString cell
                , "Index:"
                , String.fromInt properties.index
                , "tile ID:"
                , String.fromInt properties.singleTile.id
                , "large tile ID: "
                , String.fromInt properties.parentTileId
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
                |> String.join "|"
    in
    stepDebug theStep ++ " / " ++ optionsDebug


debug_collapseWithId : Cell -> TileId -> Model -> Model
debug_collapseWithId cell tileId ((Model modelDetails) as model) =
    case tileByCell modelDetails.tilemap cell |> Maybe.map .kind of
        Just (Superposition opts) ->
            if List.member tileId opts then
                let
                    tileConfig =
                        tileById tileId

                    withStep =
                        Model { modelDetails | openSteps = Collapse cell tileConfig :: modelDetails.openSteps }
                in
                processOpenSteps StopAtEmptySteps withStep

            else
                model

        _ ->
            model
