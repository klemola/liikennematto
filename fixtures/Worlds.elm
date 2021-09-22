module Worlds exposing
    ( defaultWorld
    , highComplexityWorld
    , largeWorld
    , lowComplexityWorld
    , simpleWorld
    , worldThatHasAVerticalRoadAtLeftSide
    , worldThatHasParallelRoads
    , worldWithFourWayIntersection
    , worldWithThreeWayIntersection
    )

import Model.World as World exposing (World)
import Simulation.Infrastructure as Infrastructure


defaultWorld : World
defaultWorld =
    World.empty
        |> Infrastructure.buildRoadAt ( 2, 5 )
        |> Infrastructure.buildRoadAt ( 3, 5 )
        |> Infrastructure.buildRoadAt ( 4, 5 )
        |> Infrastructure.buildRoadAt ( 5, 3 )
        |> Infrastructure.buildRoadAt ( 5, 4 )
        |> Infrastructure.buildRoadAt ( 5, 5 )


simpleWorld : World
simpleWorld =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 1 )
        |> Infrastructure.buildRoadAt ( 2, 1 )


lowComplexityWorld : World
lowComplexityWorld =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 1 )
        |> Infrastructure.buildRoadAt ( 2, 1 )
        |> Infrastructure.buildRoadAt ( 3, 1 )


highComplexityWorld : World
highComplexityWorld =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 1 )
        |> Infrastructure.buildRoadAt ( 2, 1 )
        |> Infrastructure.buildRoadAt ( 1, 2 )


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 1 )
        |> Infrastructure.buildRoadAt ( 1, 2 )
        |> Infrastructure.buildRoadAt ( 1, 3 )
        |> Infrastructure.buildRoadAt ( 1, 4 )
        |> Infrastructure.buildRoadAt ( 1, 5 )
        |> Infrastructure.buildRoadAt ( 1, 6 )
        |> Infrastructure.buildRoadAt ( 1, 7 )
        |> Infrastructure.buildRoadAt ( 1, 8 )
        |> Infrastructure.buildRoadAt ( 1, 9 )
        |> Infrastructure.buildRoadAt ( 1, 10 )


worldThatHasParallelRoads : World
worldThatHasParallelRoads =
    worldThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> Infrastructure.buildRoadAt ( 3, 1 )
        |> Infrastructure.buildRoadAt ( 3, 2 )
        |> Infrastructure.buildRoadAt ( 3, 3 )
        |> Infrastructure.buildRoadAt ( 3, 4 )
        |> Infrastructure.buildRoadAt ( 3, 5 )
        |> Infrastructure.buildRoadAt ( 3, 6 )
        |> Infrastructure.buildRoadAt ( 3, 7 )
        |> Infrastructure.buildRoadAt ( 3, 8 )
        |> Infrastructure.buildRoadAt ( 3, 9 )
        |> Infrastructure.buildRoadAt ( 3, 10 )


worldWithFourWayIntersection : World
worldWithFourWayIntersection =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 2 )
        |> Infrastructure.buildRoadAt ( 2, 1 )
        |> Infrastructure.buildRoadAt ( 2, 2 )
        |> Infrastructure.buildRoadAt ( 2, 3 )
        |> Infrastructure.buildRoadAt ( 3, 2 )


worldWithThreeWayIntersection : World
worldWithThreeWayIntersection =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 3 )
        |> Infrastructure.buildRoadAt ( 2, 3 )
        |> Infrastructure.buildRoadAt ( 3, 1 )
        |> Infrastructure.buildRoadAt ( 3, 2 )
        |> Infrastructure.buildRoadAt ( 3, 3 )
        |> Infrastructure.buildRoadAt ( 3, 4 )
        |> Infrastructure.buildRoadAt ( 3, 5 )


largeWorld : World
largeWorld =
    World.empty
        |> Infrastructure.buildRoadAt ( 1, 1 )
        |> Infrastructure.buildRoadAt ( 1, 2 )
        |> Infrastructure.buildRoadAt ( 1, 3 )
        |> Infrastructure.buildRoadAt ( 1, 4 )
        |> Infrastructure.buildRoadAt ( 1, 5 )
        |> Infrastructure.buildRoadAt ( 1, 10 )
        |> Infrastructure.buildRoadAt ( 2, 1 )
        |> Infrastructure.buildRoadAt ( 2, 5 )
        |> Infrastructure.buildRoadAt ( 2, 10 )
        |> Infrastructure.buildRoadAt ( 3, 1 )
        |> Infrastructure.buildRoadAt ( 3, 2 )
        |> Infrastructure.buildRoadAt ( 3, 3 )
        |> Infrastructure.buildRoadAt ( 3, 4 )
        |> Infrastructure.buildRoadAt ( 3, 5 )
        |> Infrastructure.buildRoadAt ( 3, 6 )
        |> Infrastructure.buildRoadAt ( 3, 7 )
        |> Infrastructure.buildRoadAt ( 3, 8 )
        |> Infrastructure.buildRoadAt ( 3, 9 )
        |> Infrastructure.buildRoadAt ( 3, 10 )
        |> Infrastructure.buildRoadAt ( 4, 1 )
        |> Infrastructure.buildRoadAt ( 4, 5 )
        |> Infrastructure.buildRoadAt ( 4, 8 )
        |> Infrastructure.buildRoadAt ( 5, 1 )
        |> Infrastructure.buildRoadAt ( 5, 5 )
        |> Infrastructure.buildRoadAt ( 5, 8 )
        |> Infrastructure.buildRoadAt ( 6, 1 )
        |> Infrastructure.buildRoadAt ( 6, 2 )
        |> Infrastructure.buildRoadAt ( 6, 3 )
        |> Infrastructure.buildRoadAt ( 6, 4 )
        |> Infrastructure.buildRoadAt ( 6, 5 )
        |> Infrastructure.buildRoadAt ( 6, 8 )
        |> Infrastructure.buildRoadAt ( 7, 1 )
        |> Infrastructure.buildRoadAt ( 7, 5 )
        |> Infrastructure.buildRoadAt ( 7, 6 )
        |> Infrastructure.buildRoadAt ( 7, 7 )
        |> Infrastructure.buildRoadAt ( 7, 8 )
        |> Infrastructure.buildRoadAt ( 7, 9 )
        |> Infrastructure.buildRoadAt ( 7, 10 )
        |> Infrastructure.buildRoadAt ( 8, 1 )
        |> Infrastructure.buildRoadAt ( 8, 5 )
        |> Infrastructure.buildRoadAt ( 8, 8 )
        |> Infrastructure.buildRoadAt ( 8, 10 )
        |> Infrastructure.buildRoadAt ( 9, 1 )
        |> Infrastructure.buildRoadAt ( 9, 5 )
        |> Infrastructure.buildRoadAt ( 9, 8 )
        |> Infrastructure.buildRoadAt ( 9, 10 )
        |> Infrastructure.buildRoadAt ( 10, 1 )
        |> Infrastructure.buildRoadAt ( 10, 2 )
        |> Infrastructure.buildRoadAt ( 10, 3 )
        |> Infrastructure.buildRoadAt ( 10, 4 )
        |> Infrastructure.buildRoadAt ( 10, 5 )
        |> Infrastructure.buildRoadAt ( 10, 6 )
        |> Infrastructure.buildRoadAt ( 10, 7 )
        |> Infrastructure.buildRoadAt ( 10, 8 )
        |> Infrastructure.buildRoadAt ( 10, 9 )
        |> Infrastructure.buildRoadAt ( 10, 10 )
