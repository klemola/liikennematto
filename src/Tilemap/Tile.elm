module Tilemap.Tile exposing
    ( Action(..)
    , FixedTileProperties
    , Tile
    , TileFSM
    , TileKind(..)
    , TileOperation(..)
    , TileState(..)
    , attemptRemove
    , fromTileConfig
    , id
    , init
    , isBuilt
    , isDynamic
    , isFixed
    , largeTileCells
    , largeTileTopLeftCell
    , transitionTimer
    , withName
    )

import Array.Extra as Array
import Audio exposing (Sound)
import Duration exposing (Duration)
import Lib.FSM as FSM exposing (FSM, State)
import Tilemap.Cell as Cell exposing (Cell, Constraints)
import Tilemap.TileConfig as TileConfig exposing (LargeTile, TileConfig, TileId)


type alias Tile =
    { kind : TileKind
    , fsm : TileFSM
    }


type TileKind
    = Unintialized
    | Fixed FixedTileProperties
    | Superposition (List TileId)


type alias FixedTileProperties =
    { id : TileId
    , name : String
    , parentTile : Maybe ( TileId, Int )
    }


type alias TileFSM =
    FSM TileState Action ()


type Action
    = PlayAudio Sound


type TileState
    = Initialized
    | Constructing
    | Generated
    | Built
    | Changing
    | Removing
    | Removed


type TileOperation
    = AddFromWFC
    | Add


init : TileKind -> Tile
init kind =
    { kind = kind
    , fsm = FSM.initialize initialized |> Tuple.first
    }


fromTileConfig : TileConfig -> Maybe ( TileId, Int ) -> TileOperation -> ( Tile, List Action )
fromTileConfig tileConfig parentTileProperties op =
    let
        initialState =
            case op of
                AddFromWFC ->
                    generated

                Add ->
                    constructing

        ( fsm, initialActions ) =
            FSM.initialize initialState

        ( tileId, name ) =
            TileConfig.tileConfigIdAndName tileConfig
    in
    ( { kind =
            Fixed
                { id = tileId
                , name = name
                , parentTile = parentTileProperties
                }
      , fsm = fsm
      }
    , initialActions
    )


attemptRemove : Tile -> ( Tile, List Action )
attemptRemove tile =
    case FSM.transitionTo (FSM.getId removing) tile.fsm of
        Ok ( nextFSM, actions ) ->
            ( Tile tile.kind nextFSM, actions )

        Err _ ->
            ( tile, [] )


withName : String -> Tile -> Tile
withName name tile =
    case tile.kind of
        Fixed props ->
            { tile | kind = Fixed { props | name = name } }

        _ ->
            tile


isFixed : Tile -> Bool
isFixed tile =
    case tile.kind of
        Fixed _ ->
            True

        _ ->
            False


isBuilt : Tile -> Bool
isBuilt tile =
    FSM.toCurrentState tile.fsm == Built


isDynamic : Tile -> Bool
isDynamic tile =
    let
        currentState =
            FSM.toCurrentState tile.fsm
    in
    currentState == Constructing || currentState == Removing


id : Tile -> Maybe TileId
id tile =
    case tile.kind of
        Fixed properties ->
            Just properties.id

        _ ->
            Nothing



--
-- Large tiles
--


largeTileTopLeftCell : Constraints a -> Cell -> Int -> LargeTile -> Maybe Cell
largeTileTopLeftCell constraints globalReferenceCell subgridIndex largeTile =
    let
        -- The large tile is a subgrid in the main grid; the tilemap
        subgridDimensions =
            { horizontalCellsAmount = largeTile.width
            , verticalCellsAmount = largeTile.height
            }

        -- The reference cell in local coordinates (a subgrid cell)
        subgridCell =
            Cell.fromArray1DIndex subgridDimensions subgridIndex
    in
    -- Find the cell of the top left cell (index 0) of the large tile subgrid,
    -- but in the space of the tilemap (local to global coordinates)
    subgridCell
        |> Maybe.map Cell.coordinates
        |> Maybe.andThen
            (\( x, y ) ->
                Cell.translateBy constraints ( -x + 1, -y + 1 ) globalReferenceCell
            )


largeTileCells : Constraints a -> Cell -> TileConfig.LargeTile -> List Cell
largeTileCells constraints topLeftCornerCell largeTile =
    let
        subgridDimensions =
            { horizontalCellsAmount = largeTile.width
            , verticalCellsAmount = largeTile.height
            }
    in
    largeTile.tiles
        |> Array.indexedMapToList
            (\index _ ->
                index
                    -- Use of unsafe function: the top left corner has been validated already
                    -- and the whole subgrid should be within tilemap bounds
                    |> Cell.fromArray1DIndexUnsafe subgridDimensions
                    |> Cell.placeIn constraints topLeftCornerCell
            )
        |> List.filterMap identity



--
-- FSM
--


transitionTimer : Duration
transitionTimer =
    Duration.milliseconds 250


transitionTimerGenerated : Duration
transitionTimerGenerated =
    Duration.milliseconds 150


initialized : State TileState Action ()
initialized =
    FSM.createState
        { id = FSM.createStateId "tile-initialized"
        , kind = Initialized
        , transitions =
            [ FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> constructing)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> generated)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


constructing : State TileState Action ()
constructing =
    FSM.createState
        { id = FSM.createStateId "tile-constructing"
        , kind = Constructing
        , transitions =
            [ FSM.createTransition
                (\_ -> built)
                [ PlayAudio Audio.BuildRoadEnd ]
                (FSM.Timer transitionTimer)
            , FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            ]
        , entryActions = [ PlayAudio Audio.BuildRoadStart ]
        , exitActions = []
        }


generated : State TileState Action ()
generated =
    FSM.createState
        { id = FSM.createStateId "tile-generated"
        , kind = Generated
        , transitions =
            [ FSM.createTransition
                (\_ -> built)
                []
                (FSM.Timer transitionTimerGenerated)
            , FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


built : State TileState Action ()
built =
    FSM.createState
        { id = FSM.createStateId "tile-built"
        , kind = Built
        , transitions =
            [ FSM.createTransition
                (\_ -> removing)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


changing : State TileState Action ()
changing =
    FSM.createState
        { id = FSM.createStateId "tile-changing"
        , kind = Changing
        , transitions =
            [ FSM.createTransition
                (\_ -> removing)
                []
                FSM.Direct
            , FSM.createTransition
                (\_ -> built)
                []
                (FSM.Condition (\_ state -> state == Changing))
            , FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            ]
        , entryActions = []
        , exitActions = []
        }


removing : State TileState Action ()
removing =
    FSM.createState
        { id = FSM.createStateId "tile-removing"
        , kind = Removing
        , transitions =
            [ FSM.createTransition
                (\_ -> removed)
                []
                (FSM.Timer transitionTimer)
            ]
        , entryActions = []
        , exitActions = []
        }


removed : State TileState Action ()
removed =
    FSM.createState
        { id = FSM.createStateId "tile-removed"
        , kind = Removed
        , transitions = []
        , entryActions = []
        , exitActions = []
        }
