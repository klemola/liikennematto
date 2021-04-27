module Worlds exposing
    ( highComplexityWorld
    , lowComplexityWorld
    , worldThatHasAVerticalRoadAtLeftSide
    , worldThatHasParallelRoads
    , worldWithIntersection
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


worldWithIntersection : World
worldWithIntersection =
    World.empty
        |> World.buildRoadAt ( 1, 2 )
        |> World.buildRoadAt ( 2, 1 )
        |> World.buildRoadAt ( 2, 2 )
        |> World.buildRoadAt ( 2, 3 )
        |> World.buildRoadAt ( 3, 2 )
