module Model.Tile exposing
    ( Action(..)
    , OrthogonalNeighbors
    , Tile
    , TileFSM
    , TileKind
    , TileOperation(..)
    , TileState(..)
    , attemptRemove
    , chooseTileKind
    , isBasicRoad
    , isBuilt
    , isCurve
    , isDeadend
    , isDynamic
    , isIntersection
    , isLotEntry
    , new
    , potentialConnections
    , transitionTimer
    , updateTileKind
    )

import Duration exposing (Duration)
import FSM exposing (FSM, State)
import Model.Geometry
    exposing
        ( OrthogonalDirection(..)
        , crossOrthogonalDirection
        , orthogonalDirections
        )
import Set exposing (Set)


type alias Tile =
    { kind : TileKind
    , fsm : TileFSM
    }


type alias TileKind =
    Int


type TileOperation
    = BuildInstantly
    | Add
    | Change


type alias TileFSM =
    FSM TileState Action ()


type Action
    = PlayAudio String


type TileState
    = Constructing
    | Built
    | Changing
    | Removing
    | Removed


new : TileKind -> TileOperation -> ( Tile, List Action )
new kind op =
    let
        ( fsm, initialActions ) =
            initializeFSM op
    in
    ( { kind = kind
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


updateTileKind : TileKind -> Tile -> ( Tile, List Action )
updateTileKind nextKind tile =
    case FSM.transitionTo (FSM.getId changing) tile.fsm of
        Ok ( nextFSM, actions ) ->
            ( Tile nextKind nextFSM, actions )

        Err _ ->
            ( tile, [] )


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


constructing : State TileState Action ()
constructing =
    FSM.createState
        { id = FSM.createStateId "tile-constructing"
        , kind = Constructing
        , transitions =
            [ FSM.createTransition
                (\_ -> built)
                [ PlayAudio "B" ]
                (FSM.Timer transitionTimer)
            , FSM.createTransition
                (\_ -> changing)
                []
                FSM.Direct
            ]
        , entryActions = [ PlayAudio "A" ]
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
        , entryActions = [ PlayAudio "Bye" ]
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


chooseTileKind : OrthogonalNeighbors -> Bool -> TileKind
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


horizontalRoad : TileKind
horizontalRoad =
    6


verticalRoad : TileKind
verticalRoad =
    9


basicRoadTiles : Set TileKind
basicRoadTiles =
    Set.fromList
        [ horizontalRoad
        , verticalRoad
        ]


isBasicRoad : Tile -> Bool
isBasicRoad tile =
    Set.member tile.kind basicRoadTiles


curveBottomRight : TileKind
curveBottomRight =
    3


curveBottomLeft : TileKind
curveBottomLeft =
    5


curveTopRight : TileKind
curveTopRight =
    10


curveTopLeft : TileKind
curveTopLeft =
    12


curveTiles : Set TileKind
curveTiles =
    Set.fromList
        [ curveTopLeft
        , curveTopRight
        , curveBottomLeft
        , curveBottomRight
        ]


isCurve : Tile -> Bool
isCurve tile =
    Set.member tile.kind curveTiles


deadendDown : TileKind
deadendDown =
    1


deadendRight : TileKind
deadendRight =
    2


deadendLeft : TileKind
deadendLeft =
    4


deadendUp : TileKind
deadendUp =
    8


deadendTiles : Set TileKind
deadendTiles =
    Set.fromList
        [ deadendDown
        , deadendRight
        , deadendLeft
        , deadendUp
        ]


isDeadend : Tile -> Bool
isDeadend tile =
    Set.member tile.kind deadendTiles


intersectionTUp : TileKind
intersectionTUp =
    7


intersectionTLeft : TileKind
intersectionTLeft =
    11


intersectionTRight : TileKind
intersectionTRight =
    13


intersectionTDown : TileKind
intersectionTDown =
    14


intersectionCross : TileKind
intersectionCross =
    15


intersectionTiles : Set TileKind
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
    Set.member tile.kind intersectionTiles


lotEntryTUp : TileKind
lotEntryTUp =
    23


lotEntryTLeft : TileKind
lotEntryTLeft =
    27


lotEntryTRight : TileKind
lotEntryTRight =
    29


lotEntryTiles : Set TileKind
lotEntryTiles =
    Set.fromList
        [ lotEntryTUp
        , lotEntryTLeft
        , lotEntryTRight
        ]


isLotEntry : Tile -> Bool
isLotEntry tile =
    Set.member tile.kind lotEntryTiles


potentialConnections : Tile -> List OrthogonalDirection
potentialConnections tile =
    let
        { kind } =
            tile
    in
    if kind == verticalRoad then
        [ Up, Down ]

    else if kind == horizontalRoad then
        [ Left, Right ]

    else if kind == curveTopRight then
        [ Left, Down ]

    else if kind == curveTopLeft then
        [ Right, Down ]

    else if kind == curveBottomRight then
        [ Left, Up ]

    else if kind == curveBottomLeft then
        [ Right, Up ]

    else if kind == deadendUp then
        [ Down ]

    else if kind == deadendRight then
        [ Left ]

    else if kind == deadendDown then
        [ Up ]

    else if kind == deadendLeft then
        [ Right ]

    else if kind == intersectionTUp || kind == lotEntryTUp then
        Up :: crossOrthogonalDirection Up

    else if kind == intersectionTRight || kind == lotEntryTRight then
        Right :: crossOrthogonalDirection Right

    else if kind == intersectionTDown then
        Down :: crossOrthogonalDirection Down

    else if kind == intersectionTLeft || kind == lotEntryTLeft then
        Left :: crossOrthogonalDirection Left

    else if kind == intersectionCross then
        orthogonalDirections

    else
        []
