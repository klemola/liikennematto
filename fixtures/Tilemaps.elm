module Tilemaps exposing
    ( tilemapThatResemblesACurve
    , tilemapThatResemblesAIntersection
    )

import Model.Tilemap as Tilemap exposing (Tilemap)


tilemapThatResemblesAIntersection : Tilemap
tilemapThatResemblesAIntersection =
    Tilemap.empty
        |> Tilemap.addTile ( 1, 1 )
        |> Tilemap.addTile ( 2, 1 )
        |> Tilemap.addTile ( 3, 1 )
        |> Tilemap.addTile ( 2, 2 )


tilemapThatResemblesACurve : Tilemap
tilemapThatResemblesACurve =
    Tilemap.empty
        |> Tilemap.addTile ( 1, 1 )
        |> Tilemap.addTile ( 2, 1 )
        |> Tilemap.addTile ( 1, 2 )
