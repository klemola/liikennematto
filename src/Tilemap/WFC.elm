module Tilemap.WFC exposing
    ( Model
    , StepEndCondition(..)
    , collapse
    , contextDebug
    , failed
    , flushPendingActions
    , fromTilemap
    , init
    , initializeArea
    , propagateConstraints
    , resetCell
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
import Data.TileSet as TileSet
    exposing
        ( allTiles
        , pairingsForSocket
        , tileById
        , tileIdsByOrthogonalMatch
        )
import Lib.OrthogonalDirection as OrthogonalDirection exposing (OrthogonalDirection)
import List.Nonempty exposing (Nonempty)
import Random exposing (Seed)
import Random.Extra
import Stack exposing (Stack)
import Tilemap.Cell as Cell exposing (Cell)
import Tilemap.Core
    exposing
        ( Tilemap
        , TilemapConfig
        , addTileInstantly
        , createTilemap
        , foldTiles
        , forAllTiles
        , getTilemapConfig
        , resetTileBySurroundings
        , setSuperpositionOptions
        , tileByCell
        )
import Tilemap.Tile as Tile exposing (Tile, TileKind(..))
import Tilemap.TileConfig as TileConfig
    exposing
        ( LargeTile
        , SingleTile
        , Socket
        , TileConfig
        , TileId
        , socketByDirection
        , socketByDirectionWithConfig
        )


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , currentCell : Maybe Cell
    , targetCell : Maybe Cell
    , openSteps : List Step
    , previousSteps : Stack ( Step, Nonempty TileId )
    , state : WFCState
    , pendingActions : List Tile.Action
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



--
-- Init
--


init : TilemapConfig -> Random.Seed -> Model
init tilemapConfig initialSeed =
    fromTilemap
        (createTilemap
            tilemapConfig
            (initTileWithSuperposition tilemapConfig)
        )
        initialSeed


initTileWithSuperposition : TilemapConfig -> Int -> Tile
initTileWithSuperposition tilemapConfig index =
    index
        |> Cell.fromArray1DIndexUnsafe tilemapConfig
        |> Cell.connectedBounds tilemapConfig
        |> tileIdsByOrthogonalMatch allTiles
        |> Superposition
        |> Tile.init


fromTilemap : Tilemap -> Random.Seed -> Model
fromTilemap tilemap initialSeed =
    Model
        { tilemap = tilemap
        , currentCell = Nothing
        , targetCell = Nothing
        , openSteps = []
        , previousSteps = Stack.empty
        , state = Solving
        , pendingActions = []
        , seed = initialSeed
        }



--
-- Automated solving
--


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position
-}
solve : TilemapConfig -> Random.Seed -> Model
solve tilemapConfig initialSeed =
    let
        nextModel =
            step StopAtSolved <| init tilemapConfig initialSeed
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
        ((Model modelDetails) as afterStep) =
            processOpenSteps endCondition model
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


stepN : StepEndCondition -> Int -> Model -> Model
stepN endCondition nTimes model =
    if nTimes == 0 then
        model

    else
        let
            stepResult =
                step endCondition model
        in
        if failed stepResult then
            stepResult

        else
            stepN endCondition (nTimes - 1) stepResult



--
-- Driven WFC
--


collapse : Cell -> Model -> Model
collapse cell model =
    let
        (Model modelDetails) =
            step StopAtEmptySteps model
    in
    case tileByCell modelDetails.tilemap cell |> Maybe.map .kind of
        Just (Superposition options) ->
            let
                ( randomTileId, nextSeed ) =
                    Random.step (Random.Extra.sample options) modelDetails.seed
            in
            case randomTileId |> Maybe.map tileById of
                Just tileConfig ->
                    let
                        withCollapsedCell =
                            Model
                                { modelDetails
                                    | openSteps = Collapse cell tileConfig :: modelDetails.openSteps
                                    , seed = nextSeed
                                    , state = Solving
                                }
                    in
                    flushOpenSteps withCollapsedCell

                _ ->
                    model

        _ ->
            model


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


initializeArea : Int -> List TileConfig -> Cell -> Model -> Model
initializeArea distance tileSet origin ((Model modelContents) as model) =
    let
        ( baseX, baseY ) =
            Cell.coordinates origin

        bounds =
            { minX = baseX - distance
            , maxX = baseX + distance
            , minY = baseY - distance
            , maxY = baseY + distance
            }
    in
    List.foldl
        (\cell nextWfcModel ->
            case tileByCell modelContents.tilemap cell of
                Just tile ->
                    case tile.kind of
                        Unintialized ->
                            resetCell tileSet cell tile.kind nextWfcModel

                        _ ->
                            nextWfcModel

                Nothing ->
                    nextWfcModel
        )
        model
        (Cell.fromArea (getTilemapConfig modelContents.tilemap) bounds)


resetCell : List TileConfig -> Cell -> TileKind -> Model -> Model
resetCell tileSet cell tileKind (Model modelContents) =
    Model { modelContents | tilemap = resetTileBySurroundings cell tileSet tileKind modelContents.tilemap }



--
-- Steps and propagation
--


processOpenSteps : StepEndCondition -> Model -> Model
processOpenSteps endCondition (Model ({ openSteps, previousSteps, tilemap, pendingActions } as modelDetails)) =
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
                Ok ( additionalSteps, tileActions, nextTilemap ) ->
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
                            , pendingActions = pendingActions ++ tileActions
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
                    case endCondition of
                        StopAtEmptySteps ->
                            { modelDetails
                                | currentCell = Nothing
                                , targetCell = Nothing
                            }

                        StopAtSolved ->
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


processStep : Tilemap -> Step -> Result WFCFailure ( List Step, List Tile.Action, Tilemap )
processStep tilemap currentStep =
    case currentStep of
        Collapse cell tileConfig ->
            case tileConfig of
                TileConfig.Large largeTile ->
                    attemptPlanLargeTilePlacement tilemap cell largeTile
                        |> Result.map (\steps -> ( steps, [], tilemap ))

                TileConfig.Single singleTile ->
                    let
                        nextSteps =
                            propagateConstraintsSteps tilemap cell

                        ( nextTilemap, tileActions ) =
                            addTileInstantly singleTile.id cell tilemap
                    in
                    Ok ( nextSteps, tileActions, nextTilemap )

        CollapseSubgridCell cell singleTile largeTileId ->
            attemptPlaceSubgridTile tilemap largeTileId cell singleTile

        PropagateConstraints from to ->
            Maybe.map2 Tuple.pair
                (tileByCell tilemap from)
                (tileByCell tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (updateSuperpositionOptions tilemap from to)
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


updateSuperpositionOptions : Tilemap -> Cell -> Cell -> ( Tile, Tile ) -> Result WFCFailure Tilemap
updateSuperpositionOptions tilemap from to tilePair =
    Cell.orthogonalDirection from to
        |> Result.fromMaybe InvalidDirection
        |> Result.andThen
            (superpositionOptionsForTile tilePair)
        |> Result.map
            (\superpositionOptions ->
                setSuperpositionOptions to superpositionOptions tilemap
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


{-| Returns true if all positions in the grid have a tile assigned
-}
solved : ModelDetails -> Bool
solved { tilemap, openSteps } =
    List.isEmpty openSteps && forAllTiles Tile.isFixed tilemap


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

                _ ->
                    ( candidates, minEntropy )
    in
    tilemap
        |> foldTiles toCandidate ( [], TileSet.allTilesAmount + 1 )
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
            getTilemapConfig tilemap

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
attemptPlaceSubgridTile : Tilemap -> TileId -> Cell -> SingleTile -> Result WFCFailure ( List Step, List Tile.Action, Tilemap )
attemptPlaceSubgridTile tilemap largeTileId cell currentTile =
    tilemap
        |> attemptTileNeighborUpdate currentTile cell
        |> Result.map
            (\neighborSteps ->
                let
                    ( nextTilemap, tileActions ) =
                        addTileInstantly currentTile.id cell tilemap
                in
                ( neighborSteps, tileActions, nextTilemap )
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
            if canDock (OrthogonalDirection.opposite nuCtx.dir) originSocket neighborTileId then
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
                                    NeighborUpdateContext dir toTile toCell tilemap steps
                            in
                            case toTile.kind of
                                Fixed tileId ->
                                    onFixed neighborUpdateContext tileId

                                Superposition options ->
                                    onSuperposition neighborUpdateContext options

                                Unintialized ->
                                    Ok steps

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


flushPendingActions : Model -> ( Model, List Tile.Action )
flushPendingActions (Model modelDetails) =
    ( Model { modelDetails | pendingActions = [] }
    , modelDetails.pendingActions
    )


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
