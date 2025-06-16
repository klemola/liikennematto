module Data.Worlds exposing
    ( defaultWorld
    , disconnectedWorld
    , highComplexityWorld
    , largeWorld
    , lotTestWorld
    , lowComplexityWorld
    , simpleWorld
    , worldWithFourWayIntersection
    , worldWithSchool
    , worldWithThreeWayIntersection
    )

import Data.Lots
import Data.Utility
    exposing
        ( addLotByEntryCell
        , placeRoadAndUpdateBuffer
        , tenByTenTilemap
        , tilemapFromCoordinates
        )
import Model.World exposing (World, createRoadNetwork)
import Tilemap.Cell as Cell
import Tilemap.DrivenWFC exposing (bufferToSuperposition)


worldFromTilemap tilemap =
    Data.Utility.worldFromTilemap tilemap
        |> createRoadNetwork tilemap


defaultWorld : World
defaultWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 2, 5 )
        , ( 3, 5 )
        , ( 4, 5 )
        , ( 5, 3 )
        , ( 5, 4 )
        , ( 5, 5 )
        ]
        |> worldFromTilemap


simpleWorld : World
simpleWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 2, 1 )
        ]
        |> worldFromTilemap


lotTestWorld : World
lotTestWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        -- a full height vertical road
        [ ( 1, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 1, 5 )
        , ( 1, 6 )
        , ( 1, 7 )
        , ( 1, 8 )
        , ( 1, 9 )
        , ( 1, 10 )

        -- enough horizontal space for anything (3 cells), then another vertical road
        , ( 4, 1 )
        , ( 4, 2 )
        , ( 4, 3 )
        , ( 4, 4 )
        , ( 4, 5 )
        , ( 4, 6 )
        , ( 4, 7 )
        , ( 4, 8 )
        , ( 4, 9 )
        , ( 4, 10 )

        -- finally horizontal roads for the last vertical ~third of the map
        -- #1
        , ( 8, 1 )
        , ( 9, 1 )
        , ( 10, 1 )

        -- #2
        , ( 8, 4 )
        , ( 9, 4 )
        , ( 10, 4 )
        ]
        |> worldFromTilemap


lowComplexityWorld : World
lowComplexityWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 3, 1 )
        ]
        |> worldFromTilemap


highComplexityWorld : World
highComplexityWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 2, 1 )
        , ( 1, 2 )
        ]
        |> worldFromTilemap


worldWithFourWayIntersection : World
worldWithFourWayIntersection =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 2 )
        , ( 2, 1 )
        , ( 2, 2 )
        , ( 2, 3 )
        , ( 3, 2 )
        ]
        |> worldFromTilemap


worldWithThreeWayIntersection : World
worldWithThreeWayIntersection =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 3 )
        , ( 2, 3 )
        , ( 3, 1 )
        , ( 3, 2 )
        , ( 3, 3 )
        , ( 3, 4 )
        , ( 3, 5 )
        ]
        |> worldFromTilemap


largeWorld : World
largeWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 1, 5 )
        , ( 1, 10 )
        , ( 2, 1 )
        , ( 2, 5 )
        , ( 2, 10 )
        , ( 3, 1 )
        , ( 3, 2 )
        , ( 3, 3 )
        , ( 3, 4 )
        , ( 3, 5 )
        , ( 3, 6 )
        , ( 3, 7 )
        , ( 3, 8 )
        , ( 3, 9 )
        , ( 3, 10 )
        , ( 4, 1 )
        , ( 4, 5 )
        , ( 4, 8 )
        , ( 5, 1 )
        , ( 5, 5 )
        , ( 5, 8 )
        , ( 6, 1 )
        , ( 6, 2 )
        , ( 6, 3 )
        , ( 6, 4 )
        , ( 6, 5 )
        , ( 6, 8 )
        , ( 7, 1 )
        , ( 7, 5 )
        , ( 7, 6 )
        , ( 7, 7 )
        , ( 7, 8 )
        , ( 7, 9 )
        , ( 7, 10 )
        , ( 8, 1 )
        , ( 8, 5 )
        , ( 8, 8 )
        , ( 8, 10 )
        , ( 9, 1 )
        , ( 9, 5 )
        , ( 9, 8 )
        , ( 9, 10 )
        , ( 10, 1 )
        , ( 10, 2 )
        , ( 10, 3 )
        , ( 10, 4 )
        , ( 10, 5 )
        , ( 10, 6 )
        , ( 10, 7 )
        , ( 10, 8 )
        , ( 10, 9 )
        , ( 10, 10 )
        ]
        |> worldFromTilemap


disconnectedWorld : World
disconnectedWorld =
    tilemapFromCoordinates
        tenByTenTilemap
        [ ( 1, 1 )
        , ( 1, 2 )
        , ( 1, 3 )
        , ( 1, 4 )
        , ( 1, 5 )
        , ( 1, 10 )
        , ( 2, 1 )
        , ( 2, 5 )
        , ( 2, 10 )
        , ( 3, 1 )
        , ( 3, 2 )
        , ( 3, 3 )
        , ( 3, 4 )
        , ( 3, 5 )
        , ( 3, 6 )
        , ( 3, 7 )
        , ( 3, 8 )
        , ( 3, 9 )
        , ( 3, 10 )
        , ( 4, 1 )
        , ( 4, 5 )
        , ( 4, 7 )
        , ( 5, 1 )
        , ( 5, 5 )
        , ( 5, 7 )
        , ( 6, 1 )
        , ( 6, 2 )
        , ( 6, 3 )
        , ( 6, 4 )
        , ( 6, 5 )
        , ( 7, 1 )
        , ( 7, 5 )
        , ( 7, 8 )
        , ( 7, 9 )
        , ( 7, 10 )
        , ( 8, 1 )
        , ( 8, 5 )
        , ( 8, 8 )
        , ( 8, 10 )
        , ( 9, 1 )
        , ( 9, 5 )
        , ( 9, 8 )
        , ( 9, 10 )
        , ( 10, 1 )
        , ( 10, 2 )
        , ( 10, 3 )
        , ( 10, 4 )
        , ( 10, 5 )
        , ( 10, 8 )
        , ( 10, 9 )
        , ( 10, 10 )
        ]
        |> worldFromTilemap


worldWithSchool : World
worldWithSchool =
    let
        tilemap =
            tilemapFromCoordinates
                tenByTenTilemap
                []
                |> placeRoadAndUpdateBuffer
                    [ ( 1, 1 )
                    , ( 1, 2 )
                    , ( 1, 3 )
                    , ( 1, 4 )
                    , ( 1, 5 )
                    , ( 1, 6 )
                    ]
                |> bufferToSuperposition

        initialWorld =
            worldFromTilemap tilemap
    in
    initialWorld
        |> addLotByEntryCell
            (Cell.fromCoordinatesUnsafe tenByTenTilemap ( 1, 4 ))
            Data.Lots.school
        |> Result.withDefault initialWorld
