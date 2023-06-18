module Editor.WFC exposing
    ( Model
    , done
    , init
    , pickTile
    , propagate
    , solve
    , tileById
    )

-- TODO:
-- do not ignore tile FMS events
--
--
--

import Common exposing (andCarry)
import Model.Cell as Cell exposing (Cell)
import Model.Geometry exposing (OrthogonalDirection(..), oppositeOrthogonalDirection, orthogonalDirections)
import Model.Tile as Tile exposing (Tile, TileId, TileKind(..))
import Model.Tilemap as Tilemap exposing (Socket, TileMeta, Tilemap, TilemapConfig)
import Random exposing (Seed)


type Model
    = Model ModelDetails


type alias ModelDetails =
    { tilemap : Tilemap
    , openSteps : List PropStep
    , seed : Seed
    }


type PropStep
    = PickTile Cell TileId
    | KeepOnlyMatching Cell (Maybe Cell)


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

        convert propagationTile =
            case propagationTile of
                Fixed tileKind ->
                    listAtWithDefault tilemapConfig.defaultTile tileKind tilemapConfig.tiles

                Superposition _ ->
                    tilemapConfig.defaultTile
    in
    -- TODO:
    -- Maybe add a new fn to allow full modification of the tilemap
    -- Tilemap.map convert tilemap
    Tilemap.empty tilemapConfig


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
    case modelDetails.openSteps of
        step :: otherSteps ->
            let
                ( additionalSteps, nextTilemap ) =
                    processStep modelDetails step modelDetails.tilemap
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
                in
                Model
                    { withPick | seed = nextSeed }

            else
                model



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

        currentSocket =
            getSocketIn dockTile dockDir
    in
    currentSocket == dockSocket


nextCandidates : ModelDetails -> List Candidate
nextCandidates { tilemap } =
    let
        tilemapConfig =
            Tilemap.config tilemap

        f cell tile ( candidates, length ) =
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
        |> Tilemap.foldr f ( [], List.length tilemapConfig.tiles + 1 )
        |> Tuple.first


pickRandom : RandomPick -> ModelDetails -> ModelDetails
pickRandom ( posRand, tileRand ) modelDetails =
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
                            List.head <| List.drop (modBy (List.length candidates) posRand) candidates
                in
                case randomCandidate of
                    Just { cell, options } ->
                        let
                            randomTileId =
                                if List.isEmpty options then
                                    Nothing

                                else
                                    List.head <| List.drop (modBy (List.length options) tileRand) options
                        in
                        case randomTileId of
                            Just tileKind ->
                                [ PickTile cell tileKind ]

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
        PickTile cell tileKind ->
            let
                tilemapConfig =
                    Tilemap.config tilemap

                mkStep dir =
                    KeepOnlyMatching cell (Cell.nextOrthogonalCell tilemapConfig dir cell)

                nextSteps =
                    List.map mkStep orthogonalDirections

                ( tile, _ ) =
                    Tile.new tileKind Tile.BuildInstantly
            in
            ( nextSteps
            , Tilemap.setTile cell tile tilemap
            )

        KeepOnlyMatching from maybeTo ->
            case
                ( Tilemap.tileAt tilemap from
                , maybeTo |> andCarry (Tilemap.tileAt tilemap)
                )
            of
                ( Just fromTile, Just ( to, toTile ) ) ->
                    case ( fromTile.kind, toTile.kind ) of
                        ( Fixed originTileId, Superposition options ) ->
                            let
                                dir =
                                    findDirection (Cell.coordinates from) (Cell.coordinates to)

                                originTile =
                                    tileById modelDetails.tilemap originTileId

                                originSocket =
                                    getSocketIn originTile dir

                                revisedOptions =
                                    List.filter
                                        (canDock modelDetails
                                            (oppositeOrthogonalDirection dir)
                                            originSocket
                                        )
                                        options

                                ( nextTile, _ ) =
                                    Tile.updateTileKind (Superposition revisedOptions) fromTile
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

        tilesAmount =
            List.length tilemapConfig.tiles
    in
    Random.pair
        (Random.int 0 tileCount)
        (Random.int 0 tilesAmount)


tileById : Tilemap -> Int -> TileMeta
tileById tilemap index =
    let
        tilemapConfig =
            Tilemap.config tilemap
    in
    -- TODO: refactor this list get by index thingy (or use the listAtWithDefault here too)
    case List.head <| List.drop index tilemapConfig.tiles of
        Nothing ->
            tilemapConfig.defaultTile

        Just aTile ->
            aTile


listAtWithDefault : a -> Int -> List a -> a
listAtWithDefault default idx list =
    case List.head <| List.drop idx list of
        Just a ->
            a

        Nothing ->
            default
