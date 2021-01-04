module Direction exposing
    ( Corner(..)
    , Direction(..)
    , Orientation(..)
    , all
    , byOrientation
    , corners
    , cross
    , fromOrientation
    , fromRadians
    , next
    , opposite
    , oppositeOrientation
    , previous
    , toDegrees
    , toOrientation
    , toRadians
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


toDegrees : Direction -> Float
toDegrees dir =
    case dir of
        Up ->
            0

        Right ->
            270

        Down ->
            180

        Left ->
            90


toRadians : Direction -> Float
toRadians =
    toDegrees >> degrees


fromRadians : Float -> Direction
fromRadians rad =
    if rad == 0 then
        Up

    else
        let
            piOver4 =
                pi / 4

            cardinalRad =
                piOver4 * (toFloat <| round (rad / piOver4))
        in
        case round cardinalRad of
            270 ->
                Right

            180 ->
                Down

            90 ->
                Left

            _ ->
                Up
