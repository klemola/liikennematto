module Editor.WFC exposing
    ( Model
    , init
    , pickTile
    , propagate
    , solve
    , stopped
    , toString
    , toTilemap
    )

import Array
import Data.TileSet exposing (defaultTile, largeTileInnerEdgeSocket, pairingsForSocket)
import List.Extra
import Model.Cell as Cell exposing (Cell, Constraints)
import Model.Geometry
    exposing
        ( OrthogonalDirection
        , oppositeOrthogonalDirection
        , orthogonalDirectionToString
        , orthogonalDirections
        )
import Model.Tile as Tile exposing (Tile, TileKind(..))
import Model.TileConfig
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
    , generationState : GenerationState
    , seed : Seed
    }


type SupervisorState
    = Generating
    | Done
    | Recovering


type GenerationState
    = Propagating
    | Failure PropagationFailure


type alias PropagationContext =
    { from : Maybe Cell
    , to : Maybe Cell
    , direction : Maybe OrthogonalDirection
    , openSteps : List PropagationStep
    }


type PropagationStep
    = PickTile Cell TileId
    | KeepMatching Cell Cell


type PropagationFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidBigTilePlacement
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
        , supervisorState = Generating
        , generationState = Propagating
        , seed = tilemapConfig.initialSeed
        }


{-| Adds a step to pick a specific tile at a specific position
-}
pickTile : Cell -> TileId -> Model -> Model
pickTile cell tileKind (Model ({ propagationContext } as modelDetails)) =
    Model
        { modelDetails
            | propagationContext =
                { propagationContext
                    | openSteps =
                        PickTile cell tileKind :: propagationContext.openSteps
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
    modelDetails.supervisorState == Done || modelDetails.supervisorState == Recovering


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

                ( fromCell, toCell, dir ) =
                    case step of
                        PickTile cell _ ->
                            ( Just cell, Nothing, Nothing )

                        KeepMatching cellA cellB ->
                            ( Just cellA
                            , Just cellB
                            , Cell.orthogonalDirection cellA cellB
                            )

                nextModelDetails =
                    case processStepResult of
                        Ok ( additionalSteps, nextTilemap ) ->
                            { modelDetails
                                | tilemap = nextTilemap
                                , generationState = Propagating
                                , propagationContext =
                                    { propagationContext
                                        | openSteps = otherSteps ++ additionalSteps
                                        , from = fromCell
                                        , to = toCell
                                        , direction = dir
                                    }
                            }

                        Err propagationFailure ->
                            { modelDetails
                                | supervisorState =
                                    -- NoPotentialMatch is a common propagation failure and does not require a recovery strategy
                                    if propagationFailure == NoPotentialMatch then
                                        Generating

                                    else
                                        Recovering
                                , generationState = Failure propagationFailure
                                , propagationContext =
                                    { propagationContext
                                        | openSteps = otherSteps
                                        , from = fromCell
                                        , to = toCell
                                        , direction = dir
                                    }
                            }
            in
            Model nextModelDetails

        [] ->
            let
                nextModelDetails =
                    if not (propagationDone modelDetails) then
                        let
                            ( randomPick, nextSeed ) =
                                Random.step (randomTileAndTileIdGen modelDetails) modelDetails.seed

                            withPick =
                                pickRandom randomPick modelDetails
                        in
                        { withPick
                            | seed = nextSeed
                            , generationState = Propagating
                        }

                    else
                        { modelDetails | supervisorState = Done }
            in
            Model nextModelDetails



--
-- Internals
--


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
        dockTile =
            tileConfigById tilemap dockTileId

        matchSocket =
            socketByDirectionWithConfig dockTile dockDir

        pairings =
            pairingsForSocket dockSocket
    in
    List.any (\pair -> pair == matchSocket) pairings


nextCandidates : ModelDetails -> List Candidate
nextCandidates { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        toCandidate cell tile ( candidates, length ) =
            case tile.kind of
                Fixed _ ->
                    ( candidates, length )

                Superposition options ->
                    let
                        currentLength =
                            List.length options
                    in
                    if currentLength > length then
                        ( candidates, length )

                    else if currentLength < length then
                        ( [ { cell = cell, options = options } ]
                        , currentLength
                        )

                    else
                        ( { cell = cell, options = options } :: candidates
                        , currentLength
                        )
    in
    tilemap
        |> Tilemap.fold toCandidate ( [], List.length tilemapConfig.tiles + 1 )
        |> Tuple.first


processStep : Tilemap -> PropagationStep -> Result PropagationFailure ( List PropagationStep, Tilemap )
processStep tilemap step =
    case step of
        PickTile cell tileId ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                stepInDirection dir =
                    -- TODO: are Fixed tile neighbors unnecessarily included here? filterMap instead?
                    Cell.nextOrthogonalCell tilemapConfig dir cell
                        |> Maybe.map (KeepMatching cell)

                nextSteps =
                    List.filterMap stepInDirection orthogonalDirections

                -- TODO: ignoring actions
                ( tile, _ ) =
                    Tile.new tileId Tile.BuildInstantly
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
                let
                    ( _, targetTile ) =
                        tilePair

                    ( tile, _ ) =
                        Tile.updateTileKind (Superposition superpositionOptions) targetTile
                in
                Tilemap.setTile to tile tilemap
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


type alias RandomPick =
    -- TODO: the (Int, Int) tuple is ambiguous, use a record with named fields instead
    ( Int, Int )


randomTileAndTileIdGen : ModelDetails -> Random.Generator RandomPick
randomTileAndTileIdGen { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        tileCount =
            -- TODO: Should random pick ingore indices that have a fixed tile?
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount

        tileConfigIds =
            List.map tileConfigId tilemapConfig.tiles

        tileConfigIdSample =
            tileConfigIds
                |> Random.Extra.sample
                |> Random.map (Maybe.withDefault 0)

        randomTilemapIndex =
            Random.int 0 tileCount
    in
    Random.pair
        randomTilemapIndex
        tileConfigIdSample


pickRandom : RandomPick -> ModelDetails -> ModelDetails
pickRandom ( indexPick, tileIdPick ) ({ propagationContext } as modelDetails) =
    let
        candidates =
            nextCandidates modelDetails

        pickRandomStep =
            if List.isEmpty candidates then
                []

            else
                let
                    randomCandidate =
                        if List.isEmpty candidates then
                            Nothing

                        else
                            let
                                randomIdx =
                                    modBy (List.length candidates) indexPick
                            in
                            List.head <| List.drop randomIdx candidates
                in
                case randomCandidate of
                    Just { cell, options } ->
                        if List.member tileIdPick options then
                            -- TODO: if large tile, attemptPlaceLargeTile
                            [ PickTile cell tileIdPick ]

                        else
                            []

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
    }


tileConfigById : Tilemap -> Int -> TileConfig
tileConfigById tilemap tileId =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    tilemapConfig.tiles
        |> List.Extra.find (\tileConfig -> tileConfigId tileConfig == tileId)
        |> Maybe.withDefault defaultTile


attemptPlaceLargeTile : Tilemap -> Cell -> LargeTile -> Result PropagationFailure Tilemap
attemptPlaceLargeTile tilemap anchorCell tile =
    let
        tilemapDimensions =
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
                        Cell.translateBy tilemapDimensions ( -x, -y ) anchorCell
                    )
    in
    case ( topLeftCornerCell, Array.get 0 tile.tiles ) of
        ( Just startCell, Just startTile ) ->
            buildLargeTile tilemap
                startCell
                subgridDimensions
                (Array.toList tile.tiles)
                startTile

        _ ->
            Err InvalidBigTilePlacement


buildLargeTile : Tilemap -> Cell -> Constraints a -> List SingleTile -> SingleTile -> Result PropagationFailure Tilemap
buildLargeTile tilemap cell subgridDimensions remainingTiles currentTile =
    let
        tilemapConfig =
            Tilemap.config tilemap

        -- Check here if the current tile can dock in the cell in all directions
        -- If the cell in the direction is part of the large tile, skip
        updatedNeighbors =
            orthogonalDirections
                |> List.filterMap
                    (\dir ->
                        let
                            originSocket =
                                socketByDirection currentTile.sockets dir
                        in
                        if originSocket == largeTileInnerEdgeSocket then
                            -- The neighbor is part of the large tile, skip
                            Nothing

                        else
                            Cell.nextOrthogonalCell tilemapConfig dir cell
                                |> Maybe.andThen
                                    (\toCell ->
                                        case Tilemap.tileAtAny tilemap toCell of
                                            Just toTile ->
                                                case toTile.kind of
                                                    Fixed tileId ->
                                                        if canDock tilemap (oppositeOrthogonalDirection dir) originSocket tileId then
                                                            -- This is fine, continue, do not include this neighbor in the updates
                                                            Nothing

                                                        else
                                                            -- TODO: Err here, can't dock!
                                                            Nothing

                                                    Superposition options ->
                                                        let
                                                            matching =
                                                                matchingSuperpositionOptions tilemap dir currentTile.id options
                                                        in
                                                        if List.isEmpty matching then
                                                            -- TODO: Err here, can't dock!
                                                            Nothing

                                                        else
                                                            let
                                                                -- TODO: ignoring actions
                                                                ( updatedTile, _ ) =
                                                                    Tile.updateTileKind (Superposition matching) toTile
                                                            in
                                                            Just ( toCell, updatedTile )

                                            Nothing ->
                                                Nothing
                                    )
                    )

        -- TODO: ignoring actions
        ( tile, _ ) =
            Tile.new currentTile.id Tile.BuildInstantly

        nextTilemap =
            Tilemap.setTile cell tile tilemap
    in
    case remainingTiles of
        [] ->
            Ok nextTilemap

        nextTile :: nextRemainingTiles ->
            buildLargeTile nextTilemap cell subgridDimensions nextRemainingTiles nextTile



--
-- Interop
--


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


toString : Model -> String
toString model =
    describeState model


describeState : Model -> String
describeState (Model modelContents) =
    let
        { propagationContext } =
            modelContents

        supervisorStateString =
            case modelContents.supervisorState of
                Generating ->
                    "generating"

                Recovering ->
                    "recovering"

                Done ->
                    "done"

        generationStateString =
            case modelContents.generationState of
                Propagating ->
                    "propagating"

                Failure propagationFailure ->
                    let
                        failureString =
                            case propagationFailure of
                                NoSuperpositionOptions ->
                                    "No superposition opts"

                                NoPotentialMatch ->
                                    "No potential match"

                                InvalidBigTilePlacement ->
                                    "Invalid big tile surroundings (no space or socket mismatch)"

                                InvalidDirection ->
                                    "Invalid direction (cell to cell)"

                                TileNotFound ->
                                    "Tile not found"
                    in
                    String.join " "
                        [ "failure:"
                        , failureString
                        ]

        parts =
            List.filterMap identity
                [ Just supervisorStateString
                , Just generationStateString
                , Maybe.map Cell.toString propagationContext.from
                , Maybe.map Cell.toString propagationContext.to
                , Maybe.map orthogonalDirectionToString propagationContext.direction
                ]
    in
    String.join " | " parts
