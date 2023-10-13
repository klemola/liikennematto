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

-- TODO:
-- do not ignore tile FMS events
--
-- Should random pick ingore indices that have a fixed tile?
--
--

import Data.TileSet exposing (defaultTile, pairingsForSocket)
import List.Extra
import Model.Cell as Cell exposing (Cell)
import Model.Geometry
    exposing
        ( OrthogonalDirection(..)
        , oppositeOrthogonalDirection
        , orthogonalDirections
        )
import Model.Tile as Tile exposing (Tile, TileKind(..))
import Model.TileConfig exposing (Socket(..), TileConfig, TileId)
import Model.Tilemap as Tilemap exposing (Tilemap, TilemapConfig)
import Random exposing (Seed)
import Random.Extra


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , openSteps : List PropagationStep
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


type PropagationStep
    = PickTile Cell TileId
    | KeepOnlyMatching Cell Cell


type PropagationFailure
    = NoSuperpositionOptions
    | NoPotentialMatch
    | InvalidDirection
    | TileNotFound


type alias Candidate =
    { cell : Cell
    , options : List TileId
    }


init : TilemapConfig -> Model
init tilemapConfig =
    Model
        { tilemap = Tilemap.empty tilemapConfig
        , openSteps = []
        , supervisorState = Generating
        , generationState = Propagating
        , seed = tilemapConfig.initialSeed
        }


{-| Adds a step to pick a specific tile at a specific position
-}
pickTile : Cell -> TileId -> Model -> Model
pickTile cell tileKind (Model model) =
    Model
        { model
            | openSteps = PickTile cell tileKind :: model.openSteps
        }


{-| Returns true if all positions in the grid have a tile assigned
-}
propagationDone : Model -> Bool
propagationDone (Model { tilemap, openSteps }) =
    List.isEmpty openSteps && Tilemap.all propagatinDonePredicate tilemap


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
propagate ((Model modelDetails) as model) =
    case modelDetails.openSteps of
        step :: otherSteps ->
            let
                processStepResult =
                    processStep modelDetails step modelDetails.tilemap

                nextModelDetails =
                    case processStepResult of
                        Ok ( additionalSteps, nextTilemap ) ->
                            { modelDetails
                                | tilemap = nextTilemap
                                , generationState = Propagating
                                , openSteps = otherSteps ++ additionalSteps
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
                                , openSteps = otherSteps
                            }
            in
            Model nextModelDetails

        [] ->
            let
                nextModelDetails =
                    if not (propagationDone model) then
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


getSocketIn : TileConfig -> OrthogonalDirection -> Socket
getSocketIn tileMeta direction =
    case direction of
        Up ->
            tileMeta.sockets.top

        Right ->
            tileMeta.sockets.right

        Down ->
            tileMeta.sockets.bottom

        Left ->
            tileMeta.sockets.left


canDock : ModelDetails -> OrthogonalDirection -> Socket -> Int -> Bool
canDock modelDetails dockDir dockSocket dockTileId =
    let
        dockTile =
            tileById modelDetails.tilemap dockTileId

        matchSocket =
            getSocketIn dockTile dockDir

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


processStep : ModelDetails -> PropagationStep -> Tilemap -> Result PropagationFailure ( List PropagationStep, Tilemap )
processStep modelDetails step tilemap =
    case step of
        PickTile cell tileId ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                mkStep dir =
                    Cell.nextOrthogonalCell tilemapConfig dir cell
                        |> Maybe.map (KeepOnlyMatching cell)

                nextSteps =
                    List.filterMap mkStep orthogonalDirections

                ( tile, _ ) =
                    Tile.new tileId Tile.BuildInstantly
            in
            Ok
                ( nextSteps
                , Tilemap.setTile cell tile tilemap
                )

        KeepOnlyMatching from to ->
            Maybe.map2 Tuple.pair
                (Tilemap.tileAtAny tilemap from)
                (Tilemap.tileAtAny tilemap to)
                |> Result.fromMaybe TileNotFound
                |> Result.andThen (findDirectionAndDock modelDetails from to)
                |> Result.map (Tuple.pair [])


findDirectionAndDock : ModelDetails -> Cell -> Cell -> ( Tile, Tile ) -> Result PropagationFailure Tilemap
findDirectionAndDock modelDetails from to tilePair =
    Cell.orthogonalDirection from to
        |> Result.fromMaybe InvalidDirection
        |> Result.andThen
            (dockTileInDirection
                modelDetails
                tilePair
            )
        |> Result.map (\tile -> Tilemap.setTile to tile modelDetails.tilemap)


dockTileInDirection : ModelDetails -> ( Tile, Tile ) -> OrthogonalDirection -> Result PropagationFailure Tile
dockTileInDirection modelDetails ( originTile, targetTile ) dir =
    case ( originTile.kind, targetTile.kind ) of
        ( Fixed originTileId, Superposition options ) ->
            let
                originTileMeta =
                    tileById modelDetails.tilemap originTileId

                originSocket =
                    getSocketIn originTileMeta dir

                revisedOptions =
                    List.filter
                        (canDock
                            modelDetails
                            (oppositeOrthogonalDirection dir)
                            originSocket
                        )
                        options
            in
            if List.isEmpty revisedOptions then
                -- let
                --     _ =
                --         Debug.log "NoSuperpositionOptions" ( originTileId, options, dir )
                -- in
                Err NoSuperpositionOptions

            else
                let
                    ( nextTile, _ ) =
                        Tile.updateTileKind (Superposition revisedOptions) originTile
                in
                Ok nextTile

        _ ->
            Err NoPotentialMatch


type alias RandomPick =
    ( Int, Int )


randomTileAndTileIdGen : ModelDetails -> Random.Generator RandomPick
randomTileAndTileIdGen { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        tileCount =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount

        tileIds =
            List.map .id tilemapConfig.tiles

        tileIdSample =
            tileIds
                |> Random.Extra.sample
                |> Random.map (Maybe.withDefault defaultTile.id)
    in
    Random.pair
        (Random.int 0 tileCount)
        tileIdSample


pickRandom : RandomPick -> ModelDetails -> ModelDetails
pickRandom ( indexPick, tileIdPick ) modelDetails =
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
                        let
                            randomTileId =
                                if List.member tileIdPick options then
                                    Just tileIdPick

                                else
                                    Nothing
                        in
                        case randomTileId of
                            Just tileId ->
                                [ PickTile cell tileId ]

                            Nothing ->
                                []

                    Nothing ->
                        []
    in
    { modelDetails
        | openSteps = pickRandomStep ++ modelDetails.openSteps
    }


tileById : Tilemap -> Int -> TileConfig
tileById tilemap tileId =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    tilemapConfig.tiles
        |> List.Extra.find (\tm -> tm.id == tileId)
        |> Maybe.withDefault defaultTile



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

                                InvalidDirection ->
                                    "Invalid direction (cell to cell)"

                                TileNotFound ->
                                    "Tile not found"
                    in
                    "failure: " ++ failureString
    in
    supervisorStateString ++ " | " ++ generationStateString
