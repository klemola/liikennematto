module Direction exposing
    ( Corner(..)
    , Direction(..)
    , Orientation(..)
    , all
    , byOrientation
    , corners
    , cross
    , fromOrientation
    , next
    , opposite
    , oppositeOrientation
    , previous
    , toOrientation
    )


type Direction
    = Up
    | Right
    | Down
    | Left


type Corner
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft



-- Room for improvement: consider moving Orientation & related fns to a separate module


type Orientation
    = Vertical
    | Horizontal


horizontal : List Direction
horizontal =
    [ Left, Right ]


vertical : List Direction
vertical =
    [ Up, Down ]


all : List Direction
all =
    vertical ++ horizontal


byOrientation : List (List Direction)
byOrientation =
    [ vertical, horizontal ]


fromOrientation : Orientation -> List Direction
fromOrientation orientation =
    case orientation of
        Vertical ->
            vertical

        Horizontal ->
            horizontal


opposite : Direction -> Direction
opposite dir =
    case dir of
        Up ->
            Down

        Right ->
            Left

        Down ->
            Up

        Left ->
            Right


toOrientation : Direction -> Orientation
toOrientation dir =
    case dir of
        Up ->
            Vertical

        Right ->
            Horizontal

        Down ->
            Vertical

        Left ->
            Horizontal


oppositeOrientation : Orientation -> Orientation
oppositeOrientation orientation =
    case orientation of
        Vertical ->
            Horizontal

        Horizontal ->
            Vertical


previous : Direction -> Direction
previous dir =
    case dir of
        Up ->
            Left

        Right ->
            Up

        Down ->
            Right

        Left ->
            Down


next : Direction -> Direction
next dir =
    case dir of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


cross : Direction -> List Direction
cross fromDir =
    case fromDir of
        Up ->
            horizontal

        Right ->
            vertical

        Down ->
            horizontal

        Left ->
            vertical


corners : List Corner
corners =
    [ TopLeft, TopRight, BottomLeft, BottomRight ]
