module Direction exposing (..)


type Direction
    = Up
    | Right
    | Down
    | Left



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


orientations : List Orientation
orientations =
    [ Vertical, Horizontal ]


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


rotationDegrees : Direction -> Float
rotationDegrees dir =
    -- the values here are based on Collage logic: counter-clockwise rotation (from "Up")
    case dir of
        Up ->
            0

        Right ->
            270

        Down ->
            180

        Left ->
            90
