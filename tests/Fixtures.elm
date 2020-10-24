module Fixtures exposing (..)

import Board exposing (Board)
import Car exposing (Car)
import Cell
import Config exposing (tileSize)
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Lot exposing (Anchor, Lot(..))
import Round exposing (Round)
import SharedState exposing (SharedState)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight


carOne : Car
carOne =
    Car.new Car.SedanA


carTwo : Car
carTwo =
    Car.new Car.SedanB


fakeRandomDirections : List Direction
fakeRandomDirections =
    [ Right, Left, Right, Down ]



-- Setups for testing Rules and Round behavior


respawnSetup : Round
respawnSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            carOne

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


connectedRoadsSetup : Round
connectedRoadsSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


disconnectedRoadsSetup : Round
disconnectedRoadsSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


curveSetup : Round
curveSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Curve TopRight) Both)
                |> Board.set ( 2, 2 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 80, 720 )
                |> Car.turn Right

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


randomTurnSetup : Round
randomTurnSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (Intersection (Yield Vertical) Crossroads)
                |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            []
    in
    Round.new board True fakeRandomDirections car otherCars


collisionSetup : Round
collisionSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            [ carTwo
                |> Car.spawn ( 80, 720 )
                |> Car.turn Right
            ]
    in
    Round.new board False fakeRandomDirections car otherCars


noCollisionSetup : Round
noCollisionSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            [ carTwo
                |> Car.spawn ( 80, 720 )
                |> Car.turn Left
            ]
    in
    Round.new board False fakeRandomDirections car otherCars


redTrafficLightsSetup : Round
redTrafficLightsSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (Intersection (Signal TrafficLight.default) Crossroads)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Right

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


greenTrafficLightsSetup : Round
greenTrafficLightsSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 1, 2 ) (Intersection (Signal TrafficLight.default) Crossroads)

        car =
            carOne
                |> Car.spawn ( 0, 720 )
                |> Car.turn Down

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


yieldSetup : Bool -> Round
yieldSetup hasPriorityTraffic =
    let
        board =
            Board.new
                |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> Board.set ( 2, 2 ) (Intersection (Yield Horizontal) (T Left))
                |> Board.set ( 2, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 0, 640 )
                |> Car.turn Right

        otherCars =
            if hasPriorityTraffic then
                [ carTwo
                    |> Car.spawn ( 80, 720 )
                    |> Car.turn Down
                ]

            else
                []
    in
    Round.new board False fakeRandomDirections car otherCars


yieldWithPriorityTrafficSetup : Round
yieldWithPriorityTrafficSetup =
    yieldSetup True


yieldWithoutPriorityTrafficSetup : Round
yieldWithoutPriorityTrafficSetup =
    yieldSetup False


stopSetup : Round
stopSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 3, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> Board.set ( 3, 2 ) (Intersection (Stop Horizontal) (T Left))
                |> Board.set ( 3, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 0, 640 )
                |> Car.turn Right
                |> Car.move

        otherCars =
            []
    in
    Round.new board False fakeRandomDirections car otherCars


yieldAfterStopSetup : Round
yieldAfterStopSetup =
    let
        board =
            Board.new
                |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)
                |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Vertical) Both)
                |> Board.set ( 2, 2 ) (Intersection (Stop Horizontal) (T Left))
                |> Board.set ( 2, 3 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 0, 640 )
                |> Car.turn Right
                |> Car.stopAtIntersection

        otherCars =
            [ carTwo
                |> Car.spawn ( 80, 720 )
                |> Car.turn Down
            ]
    in
    Round.new board False fakeRandomDirections car otherCars


lowComplexityBoard : Board
lowComplexityBoard =
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Deadend Left) Both)
        |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
        |> Board.set ( 3, 1 ) (TwoLaneRoad (Deadend Right) Both)


highComplexityBoard : Board
highComplexityBoard =
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Curve TopLeft) Both)
        |> Board.set ( 2, 1 ) (TwoLaneRoad (Deadend Right) Both)
        |> Board.set ( 1, 2 ) (TwoLaneRoad (Deadend Down) Both)


boardThatResemblesAIntersection : Board
boardThatResemblesAIntersection =
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Deadend Left) Both)
        |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
        |> Board.set ( 3, 1 ) (TwoLaneRoad (Deadend Right) Both)
        |> Board.set ( 2, 2 ) (TwoLaneRoad (Regular Horizontal) Both)


expectedIntersectionTile : Tile
expectedIntersectionTile =
    Intersection (Yield Vertical) (T Down)


boardThatResemblesACurve : Board
boardThatResemblesACurve =
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Deadend Left) Both)
        |> Board.set ( 2, 1 ) (TwoLaneRoad (Deadend Right) Both)
        |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Horizontal) Both)


boardThatHasAVerticalRoadAtLeftSide : Board
boardThatHasAVerticalRoadAtLeftSide =
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Deadend Up) Both)
        |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 3 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 4 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 5 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 6 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 7 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 8 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 9 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 1, 10 ) (TwoLaneRoad (Deadend Down) Both)


boardThatHasParallelRoads : Board
boardThatHasParallelRoads =
    boardThatHasAVerticalRoadAtLeftSide
        -- create second road
        |> Board.set ( 3, 1 ) (TwoLaneRoad (Deadend Up) Both)
        |> Board.set ( 3, 2 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 3 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 4 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 5 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 6 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 7 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 8 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 9 ) (TwoLaneRoad (Regular Vertical) Both)
        |> Board.set ( 3, 10 ) (TwoLaneRoad (Deadend Down) Both)


expectedCurveTile : Tile
expectedCurveTile =
    TwoLaneRoad (Curve TopLeft) Both


boardThatHasModifiersOnTiles : Board
boardThatHasModifiersOnTiles =
    -- A roundabout with two exits
    Board.new
        |> Board.set ( 1, 1 ) (TwoLaneRoad (Curve TopLeft) OneWay)
        |> Board.set ( 2, 1 ) (TwoLaneRoad (Regular Horizontal) Both)
        |> Board.set ( 3, 1 ) (TwoLaneRoad (Curve TopRight) OneWay)
        |> Board.set ( 1, 2 ) (TwoLaneRoad (Regular Vertical) OneWay)
        -- (2, 2) is empty
        |> Board.set ( 3, 2 ) (Intersection (Stop Horizontal) (T Right))
        |> Board.set ( 4, 2 ) (TwoLaneRoad (Deadend Right) Both)
        |> Board.set ( 5, 2 ) (TwoLaneRoad (Deadend Right) OneWay)
        |> Board.set ( 1, 3 ) (TwoLaneRoad (Curve BottomLeft) OneWay)
        |> Board.set ( 2, 3 ) (Intersection (Yield Vertical) (T Down))
        |> Board.set ( 3, 3 ) (TwoLaneRoad (Curve BottomRight) OneWay)
        |> Board.set ( 2, 4 ) (TwoLaneRoad (Deadend Down) Both)


expectedModifierTileA : Tile
expectedModifierTileA =
    TwoLaneRoad (Curve TopLeft) OneWay


expectedModifierTileB : Tile
expectedModifierTileB =
    Intersection (Stop Horizontal) (T Right)



-- Lots


oneByOneBuildingProperties : Lot.BuildingProperties
oneByOneBuildingProperties =
    { kind = Lot.ResidentialA
    , width = tileSize
    , height = tileSize
    , entryDirection = Down
    }


oneByOneLot : Lot
oneByOneLot =
    Building
        oneByOneBuildingProperties
        ( 0, tileSize * 9 )
        ( ( 1, 2 ), Up )


twoByTwoBuildingProperties : Lot.BuildingProperties
twoByTwoBuildingProperties =
    { kind = Lot.ResidentialE
    , width = tileSize * 2
    , height = tileSize * 2
    , entryDirection = Down
    }


twoByTwoLot : Lot
twoByTwoLot =
    createTwoByTwoLot ( ( 1, 3 ), Up )


createTwoByTwoLot : Anchor -> Lot
createTwoByTwoLot ( anchorCell, anchorDir ) =
    Building
        { twoByTwoBuildingProperties | entryDirection = Direction.opposite anchorDir }
        (Cell.next anchorCell anchorDir
            |> Cell.bottomLeftCorner
        )
        ( anchorCell, anchorDir )



-- SharedState/World


emptySharedState : SharedState
emptySharedState =
    SharedState.initial |> (\ss -> { ss | board = Board.new })


sharedStateWithEmptySpace : SharedState
sharedStateWithEmptySpace =
    { emptySharedState | board = boardThatHasAVerticalRoadAtLeftSide }


sharedStateWithParallelRoads : SharedState
sharedStateWithParallelRoads =
    { emptySharedState | board = boardThatHasParallelRoads }
