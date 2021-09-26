module Boards exposing
    ( boardThatResemblesACurve
    , boardThatResemblesAIntersection
    )

import Model.Board as Board exposing (Board)


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Board.empty
        |> Board.addTile ( 1, 1 )
        |> Board.addTile ( 2, 1 )
        |> Board.addTile ( 3, 1 )
        |> Board.addTile ( 2, 2 )


boardThatResemblesACurve : Board
boardThatResemblesACurve =
    Board.empty
        |> Board.addTile ( 1, 1 )
        |> Board.addTile ( 2, 1 )
        |> Board.addTile ( 1, 2 )
