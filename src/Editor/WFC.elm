module Editor.WFC exposing
    ( Model
    , currentCell
    , init
    , pickTile
    , propagate
    , propagationContextDebug
    , solve
    , stateDebug
    , stopped
    , toTilemap
    )

import Array
import Common exposing (attemptFoldList, attemptMapList)
import Data.TileSet exposing (defaultTile, largeTileInnerEdgeSocket, pairingsForSocket)
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
    = PickTile Cell TileConfig
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
    modelDetails.supervisorState == Done || modelDetails.supervisorState == Recovering


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

                { from, to, direction } =
                    stepPosition step

                nextModelDetails =
                    case processStepResult of
                        Ok ( additionalSteps, nextTilemap ) ->
                            { modelDetails
                                | tilemap = nextTilemap
                                , generationState = Propagating
                                , propagationContext =
                                    { propagationContext
                                        | openSteps = otherSteps ++ additionalSteps
                                        , from = from
                                        , to = to
                                        , direction = direction
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
                                        , from = from
                                        , to = to
                                        , direction = direction
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
        PickTile cell tileConfig ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                stepInDirection dir =
                    -- TODO: are Fixed tile neighbors unnecessarily included here? filterMap instead?
                    Cell.nextOrthogonalCell tilemapConfig dir cell
                        |> Maybe.map (KeepMatching cell)
            in
            case tileConfig of
                TileConfig.Large largeTile ->
                    attemptPlaceLargeTile tilemap cell largeTile
                        |> Result.map (Tuple.pair [])

                TileConfig.Single singleTile ->
                    let
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
    { tileConfig : TileConfig, index : Int }


randomTileAndTileIdGen : ModelDetails -> Random.Generator RandomPick
randomTileAndTileIdGen { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        tileCount =
            -- TODO: Should random pick ingore indices that have a fixed tile?
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount

        tileConfigSample =
            tilemapConfig.tiles
                |> List.filter TileConfig.isSingleTile
                |> Random.Extra.sample
                |> Random.map (Maybe.withDefault defaultTile)

        randomTilemapIndex =
            Random.int 0 tileCount
    in
    Random.map2
        (\idx tileConfig ->
            { tileConfig = tileConfig
            , index = idx
            }
        )
        randomTilemapIndex
        tileConfigSample


pickRandom : RandomPick -> ModelDetails -> ModelDetails
pickRandom randomPick ({ propagationContext, tilemap, seed } as modelDetails) =
    let
        candidates =
            nextCandidates modelDetails

        pickRandomStep =
            case candidates of
                [] ->
                    []

                singleCandidate :: [] ->
                    case
                        Random.step (Random.Extra.sample singleCandidate.options) seed
                    of
                        ( Just optionId, _ ) ->
                            [ PickTile singleCandidate.cell (tileConfigById tilemap optionId) ]

                        ( Nothing, _ ) ->
                            []

                _ ->
                    let
                        randomCandidate =
                            let
                                randomIdx =
                                    modBy (List.length candidates) randomPick.index
                            in
                            List.head <| List.drop randomIdx candidates
                    in
                    case randomCandidate of
                        Just { cell, options } ->
                            if List.member (TileConfig.tileConfigId randomPick.tileConfig) options then
                                [ PickTile cell randomPick.tileConfig ]

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
    -- TODO: optimize?
    tilemapConfig.tiles
        |> List.Extra.find (\tileConfig -> tileConfigId tileConfig == tileId)
        |> Maybe.withDefault defaultTile


attemptPlaceLargeTile : Tilemap -> Cell -> LargeTile -> Result PropagationFailure Tilemap
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
        |> Result.fromMaybe InvalidBigTilePlacement
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
                                    Err InvalidBigTilePlacement
                        )
            )
        |> Result.andThen (attemptPlaceLargeTileHelper tilemap)


attemptPlaceLargeTileHelper : Tilemap -> List ( Cell, SingleTile ) -> Result PropagationFailure Tilemap
attemptPlaceLargeTileHelper tilemap tileList =
    case tileList of
        [] ->
            Ok tilemap

        ( cell, currentTile ) :: remainingTiles ->
            let
                nextTilemapResult =
                    tilemap
                        |> attemptNeighborUpdate currentTile cell
                        |> Result.map
                            (\tilemapWithNeighborUpdate ->
                                let
                                    -- TODO: ignoring actions
                                    ( tile, _ ) =
                                        Tile.new currentTile.id Tile.BuildInstantly
                                in
                                Tilemap.setTile cell tile tilemapWithNeighborUpdate
                            )
            in
            case nextTilemapResult of
                Err _ ->
                    nextTilemapResult

                Ok nextTilemap ->
                    attemptPlaceLargeTileHelper nextTilemap remainingTiles


attemptNeighborUpdate : SingleTile -> Cell -> Tilemap -> Result PropagationFailure Tilemap
attemptNeighborUpdate currentTile cell tilemap =
    let
        tilemapConfig =
            Tilemap.config tilemap

        neighborUpdateByDirection : OrthogonalDirection -> Tilemap -> Result PropagationFailure Tilemap
        neighborUpdateByDirection dir nextTilemap =
            let
                originSocket =
                    socketByDirection currentTile.sockets dir
            in
            if originSocket == largeTileInnerEdgeSocket then
                -- The neighbor is part of the large tile = skip
                Ok nextTilemap

            else
                case Cell.nextOrthogonalCell tilemapConfig dir cell of
                    Just toCell ->
                        case Tilemap.tileAtAny nextTilemap toCell of
                            Just toTile ->
                                case toTile.kind of
                                    Fixed tileId ->
                                        if canDock nextTilemap (oppositeOrthogonalDirection dir) originSocket tileId then
                                            -- Tiles can dock, no tilemap update needed = skip
                                            Ok nextTilemap

                                        else
                                            Err InvalidBigTilePlacement

                                    Superposition options ->
                                        let
                                            matching =
                                                matchingSuperpositionOptions nextTilemap dir currentTile.id options
                                        in
                                        if List.isEmpty matching then
                                            Err InvalidBigTilePlacement

                                        else
                                            let
                                                -- TODO: ignoring actions
                                                ( updatedTile, _ ) =
                                                    Tile.updateTileKind (Superposition matching) toTile
                                            in
                                            Ok (Tilemap.setTile toCell updatedTile nextTilemap)

                            Nothing ->
                                -- Tile not found = skip
                                Ok nextTilemap

                    Nothing ->
                        -- Out of tilemap bounds = skip
                        Ok tilemap
    in
    attemptFoldList neighborUpdateByDirection tilemap orthogonalDirections



--
-- Interop and debug
--


toTilemap : Model -> Tilemap
toTilemap (Model modelDetails) =
    modelDetails.tilemap


stateDebug : Model -> String
stateDebug (Model modelDetails) =
    let
        supervisorStateString =
            case modelDetails.supervisorState of
                Generating ->
                    "generating"

                Recovering ->
                    "recovering"

                Done ->
                    "done"

        generationStateString =
            case modelDetails.generationState of
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
    in
    String.join " | " [ supervisorStateString, generationStateString ]


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
                                String.join ""
                                    [ "PickTile"
                                    , Cell.toString cell
                                    , "tile config:"
                                    , TileConfig.toString tileConfig
                                    ]

                            KeepMatching fromCell toCell ->
                                String.join "" [ "KeepMatching from:", Cell.toString fromCell, "to:", Cell.toString toCell ]
                    )
    in
    { position = position
    , openSteps = openSteps
    }
