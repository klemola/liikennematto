module Editor.WFC exposing
    ( Model
    , done
    , init
    , pickTile
    , propagate
    , solve
    , toTilemap
    )

-- TODO:
-- do not ignore tile FMS events
--
-- Should random pick ingore indices that have a fixed tile?
--
--

import Data.Tiles exposing (defaultTile, pairingsForSocket)
import List.Extra
import Model.Cell as Cell exposing (Cell)
import Model.Geometry exposing (OrthogonalDirection(..), oppositeOrthogonalDirection, orthogonalDirections)
import Model.Tile as Tile exposing (Socket, Tile, TileId, TileKind(..), TileMeta)
import Model.Tilemap as Tilemap exposing (Tilemap, TilemapConfig)
import Random exposing (Seed)
import Random.Extra


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , openSteps : List PropStep
    , seed : Seed
    }


toTilemap : Model -> Tilemap
toTilemap (Model model) =
    model.tilemap


type PropStep
    = PickTile Cell TileId
    | KeepOnlyMatching Cell Cell


type alias Candidate =
    { cell : Cell
    , options : List TileId
    }


init : TilemapConfig -> Model
init tilemapConfig =
    Model
        { tilemap = Tilemap.empty tilemapConfig
        , openSteps = []
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
done : Model -> Bool
done ((Model { tilemap }) as model) =
    let
        stopped (Model { openSteps }) =
            List.isEmpty openSteps
    in
    stopped model && Tilemap.all donePredicate tilemap


donePredicate : Tile -> Bool
donePredicate tile =
    case tile.kind of
        Superposition _ ->
            False

        Fixed _ ->
            True


{-| Tries to solve/fill the whole grid in one go by assigning a tile to each position.
-}
solve : TilemapConfig -> Tilemap
solve tilemapConfig =
    let
        (Model { tilemap }) =
            let
                nextModel =
                    propagate <| init tilemapConfig
            in
            solve_ (done <| nextModel) nextModel
    in
    tilemap


solve_ : Bool -> Model -> Model
solve_ isDone model =
    if isDone then
        model

    else
        solve_ (done model) (propagate model)


{-| Execute a single step. This can mean picking the next random tile
or propagating restrictions resulting from the last placement of a tile.
-}
propagate : Model -> Model
propagate ((Model modelDetails) as model) =
    -- let
    --     _ =
    --         Debug.log
    --             "> PROPAGATE"
    --             ( modelDetails.openSteps |> List.map debugStep, modelDetails.seed )
    -- in
    case modelDetails.openSteps of
        step :: otherSteps ->
            let
                ( additionalSteps, nextTilemap ) =
                    processStep modelDetails step modelDetails.tilemap

                -- _ =
                --     Debug.log "step" (debugStep step)
            in
            Model
                { modelDetails
                    | tilemap = nextTilemap
                    , openSteps = otherSteps ++ additionalSteps
                }

        [] ->
            if not (done model) then
                let
                    ( randomPick, nextSeed ) =
                        Random.step (randomTileAndTileIdGen modelDetails) modelDetails.seed

                    withPick =
                        pickRandom randomPick modelDetails

                    -- _ =
                    --     Debug.log "randomPick" randomPick
                in
                Model
                    { withPick | seed = nextSeed }

            else
                model



-- debugStep step =
--     case step of
--         PickTile cell tileId ->
--             "pick tile " ++ Debug.toString (Cell.coordinates cell) ++ " " ++ String.fromInt tileId
--         KeepOnlyMatching from to ->
--             "keep only matching f: " ++ Debug.toString (Cell.coordinates from) ++ " t: " ++ Debug.toString (Cell.coordinates to)
-- Internals


findDirection : ( comparable, comparable ) -> ( comparable, comparable ) -> OrthogonalDirection
findDirection ( x0, y0 ) ( x1, y1 ) =
    if x1 > x0 then
        Right

    else if y1 > y0 then
        Down

    else if y0 > y1 then
        Up

    else
        Left


getSocketIn : TileMeta -> OrthogonalDirection -> Socket
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


processStep : ModelDetails -> PropStep -> Tilemap -> ( List PropStep, Tilemap )
processStep modelDetails step tilemap =
    case step of
        PickTile cell tileId ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                mkStep dir =
                    Cell.nextOrthogonalCell tilemapConfig dir cell
                        |> Maybe.map (\toCell -> KeepOnlyMatching cell toCell)

                nextSteps =
                    List.filterMap mkStep orthogonalDirections

                ( tile, _ ) =
                    Tile.new tileId Tile.BuildInstantly
            in
            ( nextSteps
            , Tilemap.setTile cell tile tilemap
            )

        KeepOnlyMatching from to ->
            case
                ( Tilemap.tileAtAny tilemap from
                , Tilemap.tileAtAny tilemap to
                )
            of
                ( Just originTile, Just targetTile ) ->
                    case ( originTile.kind, targetTile.kind ) of
                        ( Fixed originTileId, Superposition options ) ->
                            let
                                dir =
                                    findDirection (Cell.coordinates from) (Cell.coordinates to)

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

                                ( nextTile, _ ) =
                                    Tile.updateTileKind (Superposition revisedOptions) originTile
                            in
                            ( [], Tilemap.setTile to nextTile tilemap )

                        _ ->
                            ( [], tilemap )

                _ ->
                    ( [], tilemap )


type alias RandomPick =
    ( Int, Int )


randomTileAndTileIdGen : ModelDetails -> Random.Generator ( Int, Int )
randomTileAndTileIdGen { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        tileCount =
            tilemapConfig.horizontalCellsAmount * tilemapConfig.verticalCellsAmount

        tileIds =
            List.map (\tile -> tile.id) tilemapConfig.tiles

        tileIdSample =
            tileIds
                |> Random.Extra.sample
                |> Random.map (Maybe.withDefault defaultTile.id)
    in
    Random.pair
        (Random.int 0 tileCount)
        tileIdSample


tileById : Tilemap -> Int -> TileMeta
tileById tilemap tileId =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    tilemapConfig.tiles
        |> List.Extra.find (\tm -> tm.id == tileId)
        |> Maybe.withDefault defaultTile


listAtWithDefault : a -> Int -> List a -> a
listAtWithDefault default idx list =
    case List.head <| List.drop idx list of
        Just a ->
            a

        Nothing ->
            default
