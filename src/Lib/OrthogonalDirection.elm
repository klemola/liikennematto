module Lib.OrthogonalDirection exposing
    ( OrthogonalDirection(..)
    , all
    , cross
    , fromDirection2d
    , opposite
    , toDirection2d
    , toString
    )

import Direction2d exposing (Direction2d)


type OrthogonalDirection
    = Up
    | Right
    | Down
    | Left


horizontalDirections : List OrthogonalDirection
horizontalDirections =
    [ Left, Right ]


verticalDirections : List OrthogonalDirection
verticalDirections =
    [ Up, Down ]


all : List OrthogonalDirection
all =
    verticalDirections ++ horizontalDirections


up : Direction2d.Direction2d coordinates
up =
    Direction2d.positiveY


right : Direction2d.Direction2d coordinates
right =
    Direction2d.positiveX


down : Direction2d.Direction2d coordinates
down =
    Direction2d.negativeY


left : Direction2d.Direction2d coordinates
left =
    Direction2d.negativeX


opposite : OrthogonalDirection -> OrthogonalDirection
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


cross : OrthogonalDirection -> List OrthogonalDirection
cross fromDir =
    case fromDir of
        Up ->
            horizontalDirections

        Right ->
            verticalDirections

        Down ->
            horizontalDirections

        Left ->
            verticalDirections


toDirection2d : OrthogonalDirection -> Direction2d coordinates
toDirection2d dir =
    case dir of
        Up ->
            up

        Right ->
            right

        Down ->
            down

        Left ->
            left


fromDirection2d : Direction2d coordinates -> Maybe OrthogonalDirection
fromDirection2d direction =
    if direction == Direction2d.positiveX then
        Just Right

    else if direction == Direction2d.negativeX then
        Just Left

    else if direction == Direction2d.positiveY then
        Just Up

    else if direction == Direction2d.negativeY then
        Just Down

    else
        Nothing


toString : OrthogonalDirection -> String
toString dir =
    let
        dirString =
            case dir of
                Up ->
                    "up"

                Right ->
                    "right"

                Down ->
                    "down"

                Left ->
                    "left"
    in
    "OrthogonalDirection " ++ dirString
