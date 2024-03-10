module Tilemap.Tile exposing
    ( Action(..)
    , OrthogonalNeighbors
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
    , transitionTimer
    , updateTileId
    )

import Audio exposing (Sound)
import Duration exposing (Duration)
import Lib.FSM as FSM exposing (FSM, State)
import Tilemap.TileConfig exposing (TileId)


type alias Tile =
    { kind : TileKind
    , fsm : TileFSM
    }


type TileKind
    = Fixed TileId
    | Superposition (List TileId)


type alias TileFSM =
    FSM TileState Action ()


type Action
    = PlayAudio Sound


type TileState
    = Initialized
    | Constructing
    | Built
    | Changing
    | Removing
    | Removed


type TileOperation
    = BuildInstantly
    | Add


init : TileKind -> Tile
init kind =
    { kind = kind
    , fsm = FSM.initialize initialized |> Tuple.first
    }


fromTileId : TileId -> TileOperation -> ( Tile, List Action )
fromTileId kind op =
    let
        ( fsm, initialActions ) =
            initializeFSM op
    in
    ( { kind = Fixed kind
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


updateTileId : TileId -> Tile -> ( Tile, List Action )
updateTileId nextTileId tile =
    case FSM.transitionTo (FSM.getId changing) tile.fsm of
        Ok ( nextFSM, actions ) ->
            ( { kind = Fixed nextTileId
              , fsm = nextFSM
              }
            , actions
            )

        Err _ ->
            ( tile, [] )


isFixed : Tile -> Bool
isFixed tile =
    case tile.kind of
        Fixed _ ->
            True

        Superposition _ ->
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
        Fixed tileId ->
            Just tileId

        Superposition _ ->
            Nothing



--
-- FSM
--


transitionTimer : Duration
transitionTimer =
    Duration.milliseconds 250


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
        , entryActions = [ PlayAudio Audio.DestroyRoad ]
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


initializeFSM : TileOperation -> ( TileFSM, List Action )
initializeFSM op =
    let
        initialState =
            case op of
                BuildInstantly ->
                    built

                Add ->
                    constructing
    in
    FSM.initialize initialState



--
-- Bit mask
--


type alias OrthogonalNeighbors =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    }


{-| Calculates tile number (ID) based on surrounding tiles, with terrain variation taken into account (e.g. Lots)

    Up = 2^0 = 1
    Left = 2^1 = 2
    Right = 2^2 = 4
    Down = 2^3 = 8
    Terrain modifier = 2^4 = 16

    e.g. tile bordered by tiles in Up, Left and Right directions, with the modifier on

    1*1 + 2*1 + 4*1 + 8*0 + 16 * 1 = 10111 = 23

-}
fiveBitBitmask : OrthogonalNeighbors -> Bool -> Int
fiveBitBitmask { up, left, right, down } terrainModifier =
    -- 1 * up
    boolToBinary up
        + (2 * boolToBinary left)
        + (4 * boolToBinary right)
        + (8 * boolToBinary down)
        + (16 * boolToBinary terrainModifier)


boolToBinary : Bool -> Int
boolToBinary booleanValue =
    if booleanValue then
        1

    else
        0
