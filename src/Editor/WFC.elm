module Editor.WFC exposing
    ( Model
    , currentCell
    , init
    , pickTile
    , propagateWithRecovery
    , propagationContextDebug
    , solve
    , stateDebug
    , stopped
    , toTilemap
    )

import Array
import Common exposing (attemptFoldList, attemptMapList)
import Data.TileSet exposing (defaultTile, pairingsForSocket)
import List.Extra
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
import Random.Extra


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , propagationContext : PropagationContext
    , supervisorState : SupervisorState
    , seed : Seed
    }


type SupervisorState
    = Propagating
    | Done
    | Recovering PropagationFailure


type alias PropagationContext =
    { from : Maybe Cell
    , to : Maybe Cell
    , direction : Maybe OrthogonalDirection
    , openSteps : List PropagationStep
    }


type PropagationStep
    = PickTile Cell TileConfig
    | KeepMatching Cell Cell


type PropagationFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidBigTilePlacement Cell LargeTile
    | InvalidDirection
    | TileNotFound


type alias Candidate =
    { cell : Cell
    , options : List TileId
    }


initialPropagationContext : PropagationContext
initialPropagationContext =
    { from = Nothing
    , to = Nothing
    , direction = Nothing
    , openSteps = []
    }


init : TilemapConfig -> Model
init tilemapConfig =
    Model
        { tilemap = Tilemap.empty tilemapConfig
        , propagationContext = initialPropagationContext
        , supervisorState = Propagating
        , seed = tilemapConfig.initialSeed
        }


{-| Adds a step to pick a specific tile at a specific position
-}
pickTile : Cell -> TileConfig -> Model -> Model
pickTile cell tileConfig (Model ({ propagationContext } as modelDetails)) =
    Model
        { modelDetails
            | propagationContext =
                { propagationContext
                    | openSteps =
                        PickTile cell tileConfig :: propagationContext.openSteps
                }
        }


{-| Returns true if all positions in the grid have a tile assigned
-}
propagationDone : ModelDetails -> Bool
propagationDone { tilemap, propagationContext } =
    List.isEmpty propagationContext.openSteps && Tilemap.all propagatinDonePredicate tilemap


propagatinDonePredicate : Tile -> Bool
propagatinDonePredicate tile =
    case tile.kind of
        Superposition _ ->
            False

        Fixed _ ->
            True


stopped : Model -> Bool
stopped (Model modelDetails) =
    modelDetails.supervisorState /= Propagating


currentCell : Model -> Maybe Cell
currentCell (Model modelDetails) =
    case modelDetails.propagationContext.openSteps of
        [] ->
            Nothing

        x :: _ ->
            .from <| stepPosition x


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position.
-}
solve : TilemapConfig -> Model
solve tilemapConfig =
    let
        nextModel =
            propagateWithRecovery <| init tilemapConfig
    in
    solve_ nextModel


solve_ : Model -> Model
solve_ model =
    if stopped model then
        model

    else
        solve_ (propagateWithRecovery model)


propagateWithRecovery : Model -> Model
propagateWithRecovery model =
    let
        ((Model modelDetails) as afterPropagation) =
            propagate model
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
                            case attemptRecoverFromNoSuperpositionOptions cell largeTile modelDetails.tilemap of
                                Ok ( nextSteps, tilemapWithRecovery_ ) ->
                                    let
                                        _ =
                                            Debug.log "recovered" ( modelDetails.propagationContext.openSteps, nextSteps )

                                        { propagationContext } =
                                            modelDetails
                                    in
                                    Model
                                        { modelDetails
                                            | supervisorState = Propagating
                                            , tilemap = tilemapWithRecovery_
                                            , propagationContext = { propagationContext | openSteps = nextSteps }
                                        }

                                Err _ ->
                                    afterPropagation

                -- No recovery strategy yet
                _ ->
                    afterPropagation

        _ ->
            afterPropagation


{-| Execute a single step. This can mean picking the next random tile
or propagating restrictions resulting from the last placement of a tile.
-}
propagate : Model -> Model
propagate (Model ({ propagationContext, tilemap } as modelDetails)) =
    case modelDetails.propagationContext.openSteps of
        step :: otherSteps ->
            let
                processStepResult =
                    processStep tilemap step

                { from, to, direction } =
                    stepPosition step

                basePropagationContext =
                    { propagationContext
                        | openSteps = otherSteps
                        , from = from
                        , to = to
                        , direction = direction
                    }

                nextModelDetails =
                    case processStepResult of
                        Ok ( additionalSteps, nextTilemap ) ->
                            { modelDetails
                                | tilemap = nextTilemap
                                , supervisorState = Propagating
                                , propagationContext =
                                    { basePropagationContext
                                        | openSteps = basePropagationContext.openSteps ++ additionalSteps
                                    }
                            }

                        Err propagationFailure ->
                            { modelDetails
                                | supervisorState = Recovering propagationFailure
                                , propagationContext = basePropagationContext
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
                        { modelDetails | supervisorState = Done }
            in
            Model nextModelDetails



--
-- Internals
--


tileConfigById : Tilemap -> Int -> TileConfig
tileConfigById tilemap tileId =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    -- TODO: optimize?
    tilemapConfig.tiles
        |> List.Extra.find (\tileConfig -> tileConfigId tileConfig == tileId)
        |> Maybe.withDefault defaultTile


stepPosition :
    PropagationStep
    ->
        { from : Maybe Cell
        , to : Maybe Cell
        , direction : Maybe OrthogonalDirection
        }
stepPosition step =
    case step of
        PickTile cell _ ->
            { from = Just cell, to = Nothing, direction = Nothing }

        KeepMatching cellA cellB ->
            { from = Just cellA
            , to = Just cellB
            , direction = Cell.orthogonalDirection cellA cellB
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
    List.any (\pair -> pair == matchSocket) pairings


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
                            -- TODO: are Fixed tile neighbors unnecessarily included here? filterMap instead?
                            Cell.nextOrthogonalCell tilemapConfig dir cell
                                |> Maybe.map (KeepMatching cell)

                        nextSteps =
                            -- TODO: prioritize potential large tile anchor cell if it can be one of the neighbors
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


attemptRecoverFromNoSuperpositionOptions : Cell -> LargeTile -> Tilemap -> Result String ( List PropagationStep, Tilemap )
attemptRecoverFromNoSuperpositionOptions cell largeTile tilemap =
    case Array.get largeTile.anchorIndex largeTile.tiles of
        Nothing ->
            -- This should not be possible
            Err "Anchor tile not found"

        Just anchorTileConfig ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                safeOptions =
                    List.filterMap
                        (\tc ->
                            if TileConfig.complexity tc > 0.1 then
                                Nothing

                            else
                                Just (TileConfig.tileConfigId tc)
                        )
                        tilemapConfig.tiles
            in
            applyToNeighbor
                (\nuCtx neighborTileId ->
                    let
                        originSocket =
                            socketByDirection anchorTileConfig.sockets nuCtx.dir

                        neighborTileConfig =
                            tileConfigById nuCtx.tilemap neighborTileId

                        neighborComplexity =
                            TileConfig.complexity neighborTileConfig

                        canDock_ =
                            canDock nuCtx.tilemap (oppositeOrthogonalDirection nuCtx.dir) originSocket neighborTileId
                    in
                    -- This is a heuristic that might mitigate problems with complex tiles (f.ex. the lot entry tiles)
                    if neighborComplexity > 0.7 && canDock_ then
                        case TileConfig.baseTileId neighborTileConfig of
                            Just baseTileId ->
                                Ok ( [], Tilemap.setFixedTile nuCtx.neighborCell baseTileId tilemap )

                            Nothing ->
                                Err "No suitable replacement for complex neighbor tile"

                    else if canDock_ then
                        Ok ( [], nuCtx.tilemap )

                    else
                        Err "No match for low complexity neighbor"
                )
                (\nuCtx _ ->
                    -- Nothing to do for a superposition
                    Ok ( [], nuCtx.tilemap )
                )
                cell
                -- Start from a tilemap with the recovery tile reset
                (Tilemap.setFixedTile cell 0 tilemap)


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



--
-- Random pick
--


pickRandom : ModelDetails -> ModelDetails
pickRandom ({ propagationContext, tilemap, seed } as modelDetails) =
    let
        candidates =
            nextCandidates tilemap

        randomCandidateGen =
            case candidates of
                singleCandidate :: [] ->
                    Random.constant (Just singleCandidate)

                candidatesList ->
                    Random.Extra.sample candidatesList

        ( randomCandidate, seedAfterCandidateGen ) =
            Random.step randomCandidateGen seed
    in
    case randomCandidate of
        Nothing ->
            modelDetails

        Just { cell, options } ->
            let
                randomOptionGen =
                    options
                        |> List.map (tileConfigById tilemap)
                        |> Random.Extra.sample

                ( randomOption, nextSeed ) =
                    Random.step randomOptionGen seedAfterCandidateGen

                pickRandomStep =
                    case randomOption of
                        Just randomTileConfig ->
                            [ PickTile cell randomTileConfig ]

                        Nothing ->
                            []
            in
            { modelDetails
                | propagationContext =
                    { propagationContext
                        | openSteps = pickRandomStep ++ propagationContext.openSteps
                        , from = Nothing
                        , to = Nothing
                        , direction = Nothing
                    }
                , seed = nextSeed
            }


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
                    let
                        currentEntropy =
                            List.length options
                    in
                    if currentEntropy > minEntropy then
                        ( candidates, minEntropy )

                    else if currentEntropy < minEntropy then
                        ( [ { cell = cell, options = options } ]
                        , currentEntropy
                        )

                    else
                        ( { cell = cell, options = options } :: candidates
                        , currentEntropy
                        )
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
        |> Result.fromMaybe ()
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
                                    Err ()
                        )
            )
        |> Result.andThen (attemptPlaceLargeTileHelper [] tilemap)
        |> Result.mapError (\_ -> InvalidBigTilePlacement anchorCell tile)


attemptPlaceLargeTileHelper : List PropagationStep -> Tilemap -> List ( Cell, SingleTile ) -> Result () ( List PropagationStep, Tilemap )
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


attemptSubTileNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result () ( List PropagationStep, Tilemap )
attemptSubTileNeighborUpdate currentTile cell tilemap =
    applyToNeighbor
        (\nuCtx tileId ->
            let
                originSocket =
                    socketByDirection currentTile.sockets nuCtx.dir
            in
            if canDock nuCtx.tilemap (oppositeOrthogonalDirection nuCtx.dir) originSocket tileId then
                -- Tiles can dock, no tilemap update needed = skip
                Ok ( nuCtx.steps, nuCtx.tilemap )

            else
                Err ()
        )
        (\nuCtx options ->
            let
                { dir, neighborCell, neighborTile } =
                    nuCtx

                matching =
                    matchingSuperpositionOptions nuCtx.tilemap dir currentTile.id options
            in
            if List.isEmpty matching then
                Err ()

            else
                let
                    -- TODO: ignoring actions
                    ( updatedTile, _ ) =
                        Tile.updateTileKind (Superposition matching) neighborTile
                in
                Ok
                    ( nuCtx.steps
                    , Tilemap.setTile neighborCell updatedTile nuCtx.tilemap
                    )
        )
        cell
        tilemap



--
-- Interop and debug
--


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


stateDebug : Model -> String
stateDebug (Model modelDetails) =
    case modelDetails.supervisorState of
        Propagating ->
            "propagating"

        Recovering propagationFailure ->
            let
                failureString =
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
            in
            String.join " "
                [ "recovering:"
                , failureString
                ]

        Done ->
            "done"


propagationContextDebug :
    Model
    ->
        { position : List String
        , openSteps : List String
        }
propagationContextDebug (Model modelDetails) =
    let
        { propagationContext } =
            modelDetails

        position =
            [ Maybe.map Cell.toString propagationContext.from |> Maybe.withDefault "-"
            , Maybe.map Cell.toString propagationContext.to |> Maybe.withDefault "-"
            , Maybe.map orthogonalDirectionToString propagationContext.direction |> Maybe.withDefault "-"
            ]

        openSteps =
            propagationContext.openSteps
                |> List.map
                    (\step ->
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
                    )
    in
    { position = position
    , openSteps = openSteps
    }
