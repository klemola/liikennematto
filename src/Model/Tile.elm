module Model.Tile exposing
    ( Action(..)
    , OrthogonalNeighbors
    , Tile
    , TileFSM
    , TileKind(..)
    , TileOperation(..)
    , TileState(..)
    , attemptRemove
    , chooseTileKind
    , init
    , isBasicRoad
    , isBuilt
    , isCurve
    , isDeadend
    , isDynamic
    , isFixed
    , isIntersection
    , isLotEntry
    , isPropagating
    , new
    , potentialConnections
    , transitionTimer
    , updateTileId
    )

import Audio exposing (Sound)
import Duration exposing (Duration)
import FSM exposing (FSM, State)
import Model.Geometry
    exposing
        ( OrthogonalDirection(..)
        , crossOrthogonalDirection
        , orthogonalDirections
        )
import Model.TileConfig exposing (TileId)
import Set exposing (Set)


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
    = Propagating
    | Constructing
    | Built
    | Changing
    | Removing
    | Removed


type TileOperation
    = BuildInstantly
    | Add
    | Change


init : TileKind -> Tile
init propagationTile =
    { kind = propagationTile
    , fsm = FSM.initialize propagating |> Tuple.first
    }


new : TileId -> TileOperation -> ( Tile, List Action )
new kind op =
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


isPropagating : Tile -> Bool
isPropagating tile =
    FSM.toCurrentState tile.fsm == Propagating


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



--
-- FSM
--


transitionTimer : Duration
transitionTimer =
    Duration.milliseconds 250


propagating : State TileState Action ()
propagating =
    FSM.createState
        { id = FSM.createStateId "tile-propagating"
        , kind = Propagating
        , transitions =
            [ FSM.createTransition
                (\_ -> changing)
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

                Change ->
                    changing
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


chooseTileKind : OrthogonalNeighbors -> Bool -> TileId
chooseTileKind =
    fiveBitBitmask


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



--
-- Tile configuration
--


horizontalRoad : TileId
horizontalRoad =
    6


verticalRoad : TileId
verticalRoad =
    9


basicRoadTiles : Set TileId
basicRoadTiles =
    Set.fromList
        [ horizontalRoad
        , verticalRoad
        ]


isBasicRoad : Tile -> Bool
isBasicRoad tile =
    case tile.kind of
        Fixed tileId ->
            Set.member tileId basicRoadTiles

        Superposition _ ->
            False


curveBottomRight : TileId
curveBottomRight =
    3


curveBottomLeft : TileId
curveBottomLeft =
    5


curveTopRight : TileId
curveTopRight =
    10


curveTopLeft : TileId
curveTopLeft =
    12


curveTiles : Set TileId
curveTiles =
    Set.fromList
        [ curveTopLeft
        , curveTopRight
        , curveBottomLeft
        , curveBottomRight
        ]


isCurve : Tile -> Bool
isCurve tile =
    case tile.kind of
        Fixed tileId ->
            Set.member tileId curveTiles

        Superposition _ ->
            False


deadendDown : TileId
deadendDown =
    1


deadendRight : TileId
deadendRight =
    2


deadendLeft : TileId
deadendLeft =
    4


deadendUp : TileId
deadendUp =
    8


deadendTiles : Set TileId
deadendTiles =
    Set.fromList
        [ deadendDown
        , deadendRight
        , deadendLeft
        , deadendUp
        ]


isDeadend : Tile -> Bool
isDeadend tile =
    case tile.kind of
        Fixed tileId ->
            Set.member tileId deadendTiles

        Superposition _ ->
            False


intersectionTUp : TileId
intersectionTUp =
    7


intersectionTLeft : TileId
intersectionTLeft =
    11


intersectionTRight : TileId
intersectionTRight =
    13


intersectionTDown : TileId
intersectionTDown =
    14


intersectionCross : TileId
intersectionCross =
    15


intersectionTiles : Set TileId
intersectionTiles =
    Set.fromList
        [ intersectionTUp
        , intersectionTRight
        , intersectionTDown
        , intersectionTLeft
        , intersectionCross
        ]


isIntersection : Tile -> Bool
isIntersection tile =
    case tile.kind of
        Fixed tileId ->
            Set.member tileId intersectionTiles

        Superposition _ ->
            False


lotEntryTUp : TileId
lotEntryTUp =
    23


lotEntryTLeft : TileId
lotEntryTLeft =
    27


lotEntryTRight : TileId
lotEntryTRight =
    29


lotEntryTiles : Set TileId
lotEntryTiles =
    Set.fromList
        [ lotEntryTUp
        , lotEntryTLeft
        , lotEntryTRight
        ]


isLotEntry : Tile -> Bool
isLotEntry tile =
    case tile.kind of
        Fixed tileId ->
            Set.member tileId lotEntryTiles

        Superposition _ ->
            False


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections { kind } =
    case kind of
        Fixed tileId ->
            if tileId == verticalRoad then
                [ Up, Down ]

            else if tileId == horizontalRoad then
                [ Left, Right ]

            else if tileId == curveTopRight then
                [ Left, Down ]

            else if tileId == curveTopLeft then
                [ Right, Down ]

            else if tileId == curveBottomRight then
                [ Left, Up ]

            else if tileId == curveBottomLeft then
                [ Right, Up ]

            else if tileId == deadendUp then
                [ Down ]

            else if tileId == deadendRight then
                [ Left ]

            else if tileId == deadendDown then
                [ Up ]

            else if tileId == deadendLeft then
                [ Right ]

            else if tileId == intersectionTUp || tileId == lotEntryTUp then
                Up :: crossOrthogonalDirection Up

            else if tileId == intersectionTRight || tileId == lotEntryTRight then
                Right :: crossOrthogonalDirection Right

            else if tileId == intersectionTDown then
                Down :: crossOrthogonalDirection Down

            else if tileId == intersectionTLeft || tileId == lotEntryTLeft then
                Left :: crossOrthogonalDirection Left

            else if tileId == intersectionCross then
                orthogonalDirections

            else
                []

        Superposition _ ->
            []
