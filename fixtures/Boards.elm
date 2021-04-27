module Boards exposing
    ( boardThatResemblesACurve
    , boardThatResemblesAIntersection
    , curveTile
    , intersectionTile
    )

import Board exposing (Board, Tile)
import Dict


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 3, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 2 ) Board.defaultTile
        |> Board.applyMask


intersectionTile : Tile
intersectionTile =
    14


boardThatResemblesACurve : Board
boardThatResemblesACurve =
    Dict.empty
        |> Dict.insert ( 1, 1 ) Board.defaultTile
        |> Dict.insert ( 2, 1 ) Board.defaultTile
        |> Dict.insert ( 1, 2 ) Board.defaultTile
        |> Board.applyMask


curveTile : Tile
curveTile =
    12
