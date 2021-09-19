module Worlds exposing
    ( highComplexityWorld
    , largeWorld
    , lowComplexityWorld
    , worldThatHasAVerticalRoadAtLeftSide
    , worldThatHasParallelRoads
    , worldWithFourWayIntersection
    , worldWithThreeWayIntersection
    )

import Model.World as World exposing (World)
import Simulation.WorldUpdate as WorldUpdate


lowComplexityWorld : World
lowComplexityWorld =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 1 )
        |> WorldUpdate.buildRoadAt ( 2, 1 )
        |> WorldUpdate.buildRoadAt ( 3, 1 )


highComplexityWorld : World
highComplexityWorld =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 1 )
        |> WorldUpdate.buildRoadAt ( 2, 1 )
        |> WorldUpdate.buildRoadAt ( 1, 2 )


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 1 )
        |> WorldUpdate.buildRoadAt ( 1, 2 )
        |> WorldUpdate.buildRoadAt ( 1, 3 )
        |> WorldUpdate.buildRoadAt ( 1, 4 )
        |> WorldUpdate.buildRoadAt ( 1, 5 )
        |> WorldUpdate.buildRoadAt ( 1, 6 )
        |> WorldUpdate.buildRoadAt ( 1, 7 )
        |> WorldUpdate.buildRoadAt ( 1, 8 )
        |> WorldUpdate.buildRoadAt ( 1, 9 )
        |> WorldUpdate.buildRoadAt ( 1, 10 )


worldThatHasParallelRoads : World
worldThatHasParallelRoads =
    worldThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> WorldUpdate.buildRoadAt ( 3, 1 )
        |> WorldUpdate.buildRoadAt ( 3, 2 )
        |> WorldUpdate.buildRoadAt ( 3, 3 )
        |> WorldUpdate.buildRoadAt ( 3, 4 )
        |> WorldUpdate.buildRoadAt ( 3, 5 )
        |> WorldUpdate.buildRoadAt ( 3, 6 )
        |> WorldUpdate.buildRoadAt ( 3, 7 )
        |> WorldUpdate.buildRoadAt ( 3, 8 )
        |> WorldUpdate.buildRoadAt ( 3, 9 )
        |> WorldUpdate.buildRoadAt ( 3, 10 )


worldWithFourWayIntersection : World
worldWithFourWayIntersection =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 2 )
        |> WorldUpdate.buildRoadAt ( 2, 1 )
        |> WorldUpdate.buildRoadAt ( 2, 2 )
        |> WorldUpdate.buildRoadAt ( 2, 3 )
        |> WorldUpdate.buildRoadAt ( 3, 2 )


worldWithThreeWayIntersection : World
worldWithThreeWayIntersection =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 3 )
        |> WorldUpdate.buildRoadAt ( 2, 3 )
        |> WorldUpdate.buildRoadAt ( 3, 1 )
        |> WorldUpdate.buildRoadAt ( 3, 2 )
        |> WorldUpdate.buildRoadAt ( 3, 3 )
        |> WorldUpdate.buildRoadAt ( 3, 4 )
        |> WorldUpdate.buildRoadAt ( 3, 5 )


largeWorld : World
largeWorld =
    World.empty
        |> WorldUpdate.buildRoadAt ( 1, 1 )
        |> WorldUpdate.buildRoadAt ( 1, 2 )
        |> WorldUpdate.buildRoadAt ( 1, 3 )
        |> WorldUpdate.buildRoadAt ( 1, 4 )
        |> WorldUpdate.buildRoadAt ( 1, 5 )
        |> WorldUpdate.buildRoadAt ( 1, 10 )
        |> WorldUpdate.buildRoadAt ( 2, 1 )
        |> WorldUpdate.buildRoadAt ( 2, 5 )
        |> WorldUpdate.buildRoadAt ( 2, 10 )
        |> WorldUpdate.buildRoadAt ( 3, 1 )
        |> WorldUpdate.buildRoadAt ( 3, 2 )
        |> WorldUpdate.buildRoadAt ( 3, 3 )
        |> WorldUpdate.buildRoadAt ( 3, 4 )
        |> WorldUpdate.buildRoadAt ( 3, 5 )
        |> WorldUpdate.buildRoadAt ( 3, 6 )
        |> WorldUpdate.buildRoadAt ( 3, 7 )
        |> WorldUpdate.buildRoadAt ( 3, 8 )
        |> WorldUpdate.buildRoadAt ( 3, 9 )
        |> WorldUpdate.buildRoadAt ( 3, 10 )
        |> WorldUpdate.buildRoadAt ( 4, 1 )
        |> WorldUpdate.buildRoadAt ( 4, 5 )
        |> WorldUpdate.buildRoadAt ( 4, 8 )
        |> WorldUpdate.buildRoadAt ( 5, 1 )
        |> WorldUpdate.buildRoadAt ( 5, 5 )
        |> WorldUpdate.buildRoadAt ( 5, 8 )
        |> WorldUpdate.buildRoadAt ( 6, 1 )
        |> WorldUpdate.buildRoadAt ( 6, 2 )
        |> WorldUpdate.buildRoadAt ( 6, 3 )
        |> WorldUpdate.buildRoadAt ( 6, 4 )
        |> WorldUpdate.buildRoadAt ( 6, 5 )
        |> WorldUpdate.buildRoadAt ( 6, 8 )
        |> WorldUpdate.buildRoadAt ( 7, 1 )
        |> WorldUpdate.buildRoadAt ( 7, 5 )
        |> WorldUpdate.buildRoadAt ( 7, 6 )
        |> WorldUpdate.buildRoadAt ( 7, 7 )
        |> WorldUpdate.buildRoadAt ( 7, 8 )
        |> WorldUpdate.buildRoadAt ( 7, 9 )
        |> WorldUpdate.buildRoadAt ( 7, 10 )
        |> WorldUpdate.buildRoadAt ( 8, 1 )
        |> WorldUpdate.buildRoadAt ( 8, 5 )
        |> WorldUpdate.buildRoadAt ( 8, 8 )
        |> WorldUpdate.buildRoadAt ( 8, 10 )
        |> WorldUpdate.buildRoadAt ( 9, 1 )
        |> WorldUpdate.buildRoadAt ( 9, 5 )
        |> WorldUpdate.buildRoadAt ( 9, 8 )
        |> WorldUpdate.buildRoadAt ( 9, 10 )
        |> WorldUpdate.buildRoadAt ( 10, 1 )
        |> WorldUpdate.buildRoadAt ( 10, 2 )
        |> WorldUpdate.buildRoadAt ( 10, 3 )
        |> WorldUpdate.buildRoadAt ( 10, 4 )
        |> WorldUpdate.buildRoadAt ( 10, 5 )
        |> WorldUpdate.buildRoadAt ( 10, 6 )
        |> WorldUpdate.buildRoadAt ( 10, 7 )
        |> WorldUpdate.buildRoadAt ( 10, 8 )
        |> WorldUpdate.buildRoadAt ( 10, 9 )
        |> WorldUpdate.buildRoadAt ( 10, 10 )
