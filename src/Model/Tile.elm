module Model.Tile exposing
    ( Action
    , Tile
    , TileKind
    , TileOperation(..)
    , TileState(..)
    , chooseTileKind
    , curveTopLeft
    , isBasicRoad
    , isCurve
    , isDeadend
    , isIntersection
    , isRemoved
    , new
    , potentialConnections
    , tileSize
    , transitionTimer
    )

import Duration exposing (Duration)
import Length exposing (Length)
import Model.FSM as FSM exposing (FSM, State)
import Model.OrthogonalDirection
    exposing
        ( OrthogonalDirection(..)
        , crossOrthogonalDirection
        , orthogonalDirections
        )
import Set exposing (Set)


tileSize : Length
tileSize =
    Length.meters 16


type alias Tile =
    { kind : TileKind
    , fsm : TileFSM
    }


type alias TileKind =
    Int


type TileOperation
    = Add
    | Remove
    | Change


type alias TileFSM =
    FSM TileState Action


type Action
    = RemoveEffects


type TileState
    = Constructing
    | Built
    | Removing
    | Removed


new : TileKind -> TileOperation -> Tile
new kind op =
    { kind = kind
    , fsm = initializeFSM op
    }


isRemoved : Tile -> Bool
isRemoved tile =
    FSM.currentState tile.fsm == Removed



--
-- FSM
--


transitionTimer : Duration
transitionTimer =
    Duration.milliseconds 250


constructing : State TileState Action
constructing =
    FSM.createState
        (FSM.createStateId "tile-constructing")
        Constructing
        [ FSM.createTransition
            (\_ -> built)
            []
            (FSM.Timer transitionTimer)
        ]


built : State TileState Action
built =
    FSM.createState
        (FSM.createStateId "tile-built")
        Built
        [ FSM.createTransition
            (\_ -> removing)
            []
            FSM.Direct
        ]


removing : State TileState Action
removing =
    FSM.createState
        (FSM.createStateId "tile-removing")
        Removing
        [ FSM.createTransition
            (\_ -> removed)
            []
            (FSM.Timer transitionTimer)
        ]


removed : State TileState Action
removed =
    FSM.createState
        (FSM.createStateId "tile-removed")
        Removed
        []


initializeFSM : TileOperation -> TileFSM
initializeFSM op =
    let
        initialState =
            case op of
                Add ->
                    constructing

                Remove ->
                    removing

                Change ->
                    -- the "Change" operation should not have a transition; skip straight to the idle state
                    built
    in
    FSM.createFSM initialState



--
-- Bit mask
--


type alias OrthogonalNeighbors =
    { up : Bool
    , left : Bool
    , right : Bool
    , down : Bool
    }


chooseTileKind : OrthogonalNeighbors -> TileKind
chooseTileKind neighbors =
    fourBitBitmask neighbors


{-| Calculates tile number (ID) based on surrounding tiles

    Up = 2^0 = 1
    Left = 2^1 = 2
    Right = 2^2 = 4
    Down = 2^3 = 8

    e.g. tile bordered by tiles in Up and Right directions 1*1 + 2*0 + 4*1 + 8*0 = 0101 = 5

-}
fourBitBitmask : OrthogonalNeighbors -> Int
fourBitBitmask { up, left, right, down } =
    1 * boolToBinary up + 2 * boolToBinary left + 4 * boolToBinary right + 8 * boolToBinary down


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

    else if kind == intersectionTUp then
        Up :: crossOrthogonalDirection Up

    else if kind == intersectionTRight then
        Right :: crossOrthogonalDirection Right

    else if kind == intersectionTDown then
        Down :: crossOrthogonalDirection Down

    else if kind == intersectionTLeft then
        Left :: crossOrthogonalDirection Left

    else if kind == intersectionCross then
        orthogonalDirections

    else
        []
