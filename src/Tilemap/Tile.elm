module Tilemap.Tile exposing
    ( Action(..)
    , Tile
    , TileFSM
    , TileKind(..)
    , TileOperation(..)
    , TileState(..)
    , attemptRemove
    , fromTileId
    , id
    , init
    , isBuilt
    , isDynamic
    , isFixed
    , isSuperposition
    , transitionTimer
    , transitionTimerShort
    )

import Audio exposing (Sound)
import Duration exposing (Duration)
import Lib.FSM as FSM exposing (FSM, State)
import Tilemap.Cell exposing (Cell)
import Tilemap.TileConfig exposing (TileId)


type alias Tile =
    { kind : TileKind
    , fsm : TileFSM
    }


type TileKind
    = Unintialized
    | Fixed FixedTileProperties
    | Superposition (List TileId)


type alias FixedTileProperties =
    { id : TileId, parentTile : Maybe ( TileId, Int ) }


type alias TileFSM =
    FSM TileState Action ()


type Action
    = PlayAudio Sound
    | OnRemoved Cell


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


fromTileId : TileId -> Maybe ( TileId, Int ) -> TileOperation -> ( Tile, List Action )
fromTileId tileId parentTileProperties op =
    let
        initialState =
            case op of
                AddFromWFC ->
                    generated

                Add ->
                    constructing

        ( fsm, initialActions ) =
            FSM.initialize initialState
    in
    ( { kind =
            Fixed
                { id = tileId
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


isSuperposition : Tile -> Bool
isSuperposition tile =
    case tile.kind of
        Superposition _ ->
            True

        _ ->
            False


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
-- FSM
--


transitionTimer : Duration
transitionTimer =
    Duration.milliseconds 250


transitionTimerShort : Duration
transitionTimerShort =
    Duration.milliseconds 100


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
                (FSM.Timer transitionTimerShort)
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
