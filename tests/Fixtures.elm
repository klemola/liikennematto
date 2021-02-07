module Fixtures exposing (..)

import Angle
import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Corner(..), OrthogonalDirection(..))
import Config exposing (tileSize)
import Dict
import Direction2d
import Geometry
import Lot exposing (Anchor, Lot)
import Random
import Round exposing (Round)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , Orientation(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight
import World exposing (World)


seed : Random.Seed
seed =
    Random.initialSeed 42


carOne : Car
carOne =
    Car.new Car.SedanA


carTwo : Car
carTwo =
    Car.new Car.SedanB


fakeRandomDirections : List Cell.OrthogonalDirection
fakeRandomDirections =
    [ Right, Left, Right, Down ]



-- Setups for testing Rules and Round behavior


respawnSetup : Round
respawnSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            carOne

        otherCars =
            []
    in
    Round.new world seed car otherCars


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round.new world seed car otherCars


disconnectedRoadsSetup : Round
disconnectedRoadsSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round.new world seed car otherCars


curveSetup : Round
curveSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Curve TopRight) Both)
                |> World.withTileAt ( 2, 2 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            spawn carOne ( 80, 720 ) Right

        otherCars =
            []
    in
    Round.new world seed car otherCars


collisionSetup : Round
collisionSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            [ spawn carTwo ( 80, 720 ) Right
            ]
    in
    Round.new world seed car otherCars


noCollisionSetup : Round
noCollisionSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            [ spawn carTwo ( 80, 720 ) Left
            ]
    in
    Round.new world seed car otherCars


redTrafficLightsSetup : Round
redTrafficLightsSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (Intersection (Signal TrafficLight.default) Crossroads)

        car =
            spawn carOne ( 0, 720 ) Right

        otherCars =
            []
    in
    Round.new world seed car otherCars


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 1, 2 ) (Intersection (Signal TrafficLight.default) Crossroads)

        car =
            spawn carOne ( 0, 720 ) Down

        otherCars =
            []
    in
    Round.new world seed car otherCars


yieldSetup : Bool -> Round
yieldSetup hasPriorityTraffic =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> World.withTileAt ( 2, 2 ) (Intersection (Yield Horizontal) (T Left))
                |> World.withTileAt ( 2, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            spawn carOne ( 0, 640 ) Right

        otherCars =
            if hasPriorityTraffic then
                [ spawn carTwo ( 80, 720 ) Down
                ]

            else
                []
    in
    Round.new world seed car otherCars


yieldWithPriorityTrafficSetup : Round
yieldWithPriorityTrafficSetup =
    yieldSetup True


yieldWithoutPriorityTrafficSetup : Round
yieldWithoutPriorityTrafficSetup =
    yieldSetup False


stopSetup : Round
stopSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 3, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> World.withTileAt ( 3, 2 ) (Intersection (Stop Horizontal) (T Left))
                |> World.withTileAt ( 3, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            spawn carOne ( 0, 640 ) Right
                |> Car.move

        otherCars =
            []
    in
    Round.new world seed car otherCars


yieldAfterStopSetup : Round
yieldAfterStopSetup =
    let
        world =
            World.new
                |> World.withTileAt ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> World.withTileAt ( 2, 2 ) (Intersection (Stop Horizontal) (T Left))
                |> World.withTileAt ( 2, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            spawn carOne ( 0, 640 ) Right
                |> Car.stopAtIntersection

        otherCars =
            [ spawn carTwo ( 80, 720 ) Down
            ]
    in
    Round.new world seed car otherCars


spawn : Car -> ( Float, Float ) -> OrthogonalDirection -> Car
spawn car ( x, y ) direction =
    { car
        | position = Geometry.pointFromPosition (Geometry.LMEntityPositionUnitless x y)
        , rotation =
            direction
                |> Cell.orthogonalDirectionToLmDirection
                |> Direction2d.toAngle
    }



-- Boards


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Deadend Left) Both )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 1 ), TwoLaneRoad (Deadend Right) Both )
        , ( ( 2, 2 ), TwoLaneRoad (Regular Horizontal) Both )
        ]


boardThatHasModifiersOnTiles : Board
boardThatHasModifiersOnTiles =
    -- A roundabout with two exits
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) OneWay )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 1 ), TwoLaneRoad (Curve TopRight) OneWay )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) OneWay )

        -- (2, 2) is empty
        , ( ( 3, 2 ), Intersection (Stop Horizontal) (T Right) )
        , ( ( 4, 2 ), TwoLaneRoad (Deadend Right) Both )
        , ( ( 5, 2 ), TwoLaneRoad (Deadend Right) OneWay )
        , ( ( 1, 3 ), TwoLaneRoad (Curve BottomLeft) OneWay )
        , ( ( 2, 3 ), Intersection (Yield Vertical) (T Down) )
        , ( ( 3, 3 ), TwoLaneRoad (Curve BottomRight) OneWay )
        , ( ( 2, 4 ), TwoLaneRoad (Deadend Down) Both )
        ]


modifierTileA : Tile
modifierTileA =
    TwoLaneRoad (Curve TopLeft) OneWay


modifierTileB : Tile
modifierTileB =
    Intersection (Stop Horizontal) (T Right)


intersectionTile : Tile
intersectionTile =
    Intersection (Yield Vertical) (T Down)


boardThatResemblesACurve : Board
boardThatResemblesACurve =
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Deadend Left) Both )
        , ( ( 2, 1 ), TwoLaneRoad (Deadend Right) Both )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Horizontal) Both )
        ]


curveTile : Tile
curveTile =
    TwoLaneRoad (Curve TopLeft) Both



-- Worlds


lowComplexityWorld : World
lowComplexityWorld =
    World.new
        |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Deadend Left) Both)
        |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
        |> World.withTileAt ( 3, 1 ) (TwoLaneRoad (Deadend Right) Both)


highComplexityWorld : World
highComplexityWorld =
    World.new
        |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Curve TopLeft) Both)
        |> World.withTileAt ( 2, 1 ) (TwoLaneRoad (Deadend Right) Both)
        |> World.withTileAt ( 1, 2 ) (TwoLaneRoad (Deadend Down) Both)


worldThatHasAVerticalRoadAtLeftSide : World
worldThatHasAVerticalRoadAtLeftSide =
    World.new
        |> World.withTileAt ( 1, 1 ) (TwoLaneRoad (Deadend Up) Both)
        |> World.withTileAt ( 1, 2 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 3 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 4 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 5 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 6 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 7 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 8 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 9 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 1, 10 ) (TwoLaneRoad (Deadend Down) Both)


worldThatHasParallelRoads : World
worldThatHasParallelRoads =
    worldThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> World.withTileAt ( 3, 1 ) (TwoLaneRoad (Deadend Up) Both)
        |> World.withTileAt ( 3, 2 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 3 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 4 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 5 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 6 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 7 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 8 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 9 ) (TwoLaneRoad (Regular Vertical) Both)
        |> World.withTileAt ( 3, 10 ) (TwoLaneRoad (Deadend Down) Both)



-- Lots


oneByOneNewLot : Lot.NewLot
oneByOneNewLot =
    { content =
        { kind = Lot.ResidentialA
        , entryDirection = Down
        }
    , width = tileSize
    , height = tileSize
    }


oneByOneLot : Lot
oneByOneLot =
    { content = oneByOneNewLot.content
    , width = oneByOneNewLot.width
    , height = oneByOneNewLot.height
    , position = Geometry.pointFromPosition (Geometry.LMEntityPositionUnitless 0 (tileSize * 9))
    , anchor = ( ( 1, 2 ), Up )
    }


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSize * 2
    , height = tileSize * 2
    }


twoByTwoLot : Lot
twoByTwoLot =
    createTwoByTwoLot ( ( 1, 3 ), Up )


createTwoByTwoLot : Anchor -> Lot
createTwoByTwoLot ( anchorCell, anchorDir ) =
    let
        content =
            twoByTwoNewLot.content
    in
    { content = { content | entryDirection = Cell.oppositeOrthogonalDirection anchorDir }
    , width = twoByTwoNewLot.width
    , height = twoByTwoNewLot.height
    , position =
        anchorCell
            |> Cell.next anchorDir
            |> Cell.bottomLeftCorner
    , anchor = ( anchorCell, anchorDir )
    }
