module WorldTests exposing (suite)

import Common exposing (boundingBoxWithDimensions)
import Data.Lots exposing (school)
import Data.Worlds
    exposing
        ( worldThatHasAVerticalRoadAtLeftSide
        , worldThatHasParallelRoads
        )
import Expect
import Length
import Lib.Collection as Collection exposing (initialId)
import Model.World as World
import Point2d
import Simulation.Lot as Lot
import Test exposing (Test, describe, test)
import Tilemap.Cell as Cell
import Tilemap.Core exposing (getTilemapConfig)


suite : Test
suite =
    describe "World"
        []
