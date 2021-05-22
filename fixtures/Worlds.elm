module Worlds exposing
    ( highComplexityWorld
    , largeWorld
    , lowComplexityWorld
    , worldThatHasAVerticalRoadAtLeftSide
    , worldThatHasParallelRoads
    , worldWithFourWayIntersection
    , worldWithThreeWayIntersection
    )

import World exposing (World)


lowComplexityWorld : World
lowComplexityWorld =
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 3, 1 )


highComplexityWorld : World
highComplexityWorld =
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 1, 2 )


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 1, 3 )
        |> World.buildRoadAt ( 1, 4 )
        |> World.buildRoadAt ( 1, 5 )
        |> World.buildRoadAt ( 1, 6 )
        |> World.buildRoadAt ( 1, 7 )
        |> World.buildRoadAt ( 1, 8 )
        |> World.buildRoadAt ( 1, 9 )
        |> World.buildRoadAt ( 1, 10 )


worldThatHasParallelRoads : World
worldThatHasParallelRoads =
    worldThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> World.buildRoadAt ( 3, 1 )
        |> World.buildRoadAt ( 3, 2 )
        |> World.buildRoadAt ( 3, 3 )
        |> World.buildRoadAt ( 3, 4 )
        |> World.buildRoadAt ( 3, 5 )
        |> World.buildRoadAt ( 3, 6 )
        |> World.buildRoadAt ( 3, 7 )
        |> World.buildRoadAt ( 3, 8 )
        |> World.buildRoadAt ( 3, 9 )
        |> World.buildRoadAt ( 3, 10 )


worldWithFourWayIntersection : World
worldWithFourWayIntersection =
    World.empty
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 2, 2 )
        |> World.buildRoadAt ( 2, 3 )
        |> World.buildRoadAt ( 3, 2 )


worldWithThreeWayIntersection : World
worldWithThreeWayIntersection =
    World.empty
        |> World.buildRoadAt ( 1, 3 )
        |> World.buildRoadAt ( 2, 3 )
        |> World.buildRoadAt ( 3, 1 )
        |> World.buildRoadAt ( 3, 2 )
        |> World.buildRoadAt ( 3, 3 )
        |> World.buildRoadAt ( 3, 4 )
        |> World.buildRoadAt ( 3, 5 )


largeWorld : World
largeWorld =
    World.empty
        |> World.buildRoadAt ( 1, 1 )
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 1, 3 )
        |> World.buildRoadAt ( 1, 4 )
        |> World.buildRoadAt ( 1, 5 )
        |> World.buildRoadAt ( 1, 10 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 2, 5 )
        |> World.buildRoadAt ( 2, 10 )
        |> World.buildRoadAt ( 3, 1 )
        |> World.buildRoadAt ( 3, 2 )
        |> World.buildRoadAt ( 3, 3 )
        |> World.buildRoadAt ( 3, 4 )
        |> World.buildRoadAt ( 3, 5 )
        |> World.buildRoadAt ( 3, 6 )
        |> World.buildRoadAt ( 3, 7 )
        |> World.buildRoadAt ( 3, 8 )
        |> World.buildRoadAt ( 3, 9 )
        |> World.buildRoadAt ( 3, 10 )
        |> World.buildRoadAt ( 4, 1 )
        |> World.buildRoadAt ( 4, 5 )
        |> World.buildRoadAt ( 4, 8 )
        |> World.buildRoadAt ( 5, 1 )
        |> World.buildRoadAt ( 5, 5 )
        |> World.buildRoadAt ( 5, 8 )
        |> World.buildRoadAt ( 6, 1 )
        |> World.buildRoadAt ( 6, 2 )
        |> World.buildRoadAt ( 6, 3 )
        |> World.buildRoadAt ( 6, 4 )
        |> World.buildRoadAt ( 6, 5 )
        |> World.buildRoadAt ( 6, 8 )
        |> World.buildRoadAt ( 7, 1 )
        |> World.buildRoadAt ( 7, 5 )
        |> World.buildRoadAt ( 7, 6 )
        |> World.buildRoadAt ( 7, 7 )
        |> World.buildRoadAt ( 7, 8 )
        |> World.buildRoadAt ( 7, 9 )
        |> World.buildRoadAt ( 7, 10 )
        |> World.buildRoadAt ( 8, 1 )
        |> World.buildRoadAt ( 8, 5 )
        |> World.buildRoadAt ( 8, 8 )
        |> World.buildRoadAt ( 8, 10 )
        |> World.buildRoadAt ( 9, 1 )
        |> World.buildRoadAt ( 9, 5 )
        |> World.buildRoadAt ( 9, 8 )
        |> World.buildRoadAt ( 9, 10 )
        |> World.buildRoadAt ( 10, 1 )
        |> World.buildRoadAt ( 10, 2 )
        |> World.buildRoadAt ( 10, 3 )
        |> World.buildRoadAt ( 10, 4 )
        |> World.buildRoadAt ( 10, 5 )
        |> World.buildRoadAt ( 10, 6 )
        |> World.buildRoadAt ( 10, 7 )
        |> World.buildRoadAt ( 10, 8 )
        |> World.buildRoadAt ( 10, 9 )
        |> World.buildRoadAt ( 10, 10 )
