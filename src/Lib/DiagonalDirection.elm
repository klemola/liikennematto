module Lib.DiagonalDirection exposing (DiagonalDirection(..), all)


type DiagonalDirection
    = TopRight
    | TopLeft
    | BottomRight
    | BottomLeft


all : List DiagonalDirection
all =
    [ TopLeft, TopRight, BottomLeft, BottomRight ]
