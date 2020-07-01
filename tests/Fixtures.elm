module Fixtures exposing (..)

import Board
import Car exposing (Car)
import Direction exposing (Direction(..), Orientation(..))
import Round exposing (Round)
import Tile exposing (CurveKind(..), IntersectionControl(..), IntersectionShape(..), RoadKind(..), Tile(..), TrafficDirection(..))
import TrafficLight


carOne : Car
carOne =
    Car.new Car.Sedan1


carTwo : Car
carTwo =
    Car.new Car.Sedan2


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
                |> Car.spawn ( 1, 1 )
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
                |> Car.spawn ( 1, 1 )
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
                |> Board.set ( 3, 1 ) Terrain
                |> Board.set ( 2, 2 ) (TwoLaneRoad (Regular Vertical) Both)

        car =
            carOne
                |> Car.spawn ( 2, 1 )
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
                |> Car.spawn ( 1, 1 )
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
                |> Car.spawn ( 1, 1 )
                |> Car.turn Right

        otherCars =
            [ carTwo
                |> Car.spawn ( 2, 1 )
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
                |> Car.spawn ( 1, 1 )
                |> Car.turn Right

        otherCars =
            [ carTwo
                |> Car.spawn ( 2, 1 )
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
                |> Car.spawn ( 1, 1 )
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
                |> Car.spawn ( 1, 1 )
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
                |> Car.spawn ( 1, 2 )
                |> Car.turn Right

        otherCars =
            if hasPriorityTraffic then
                [ carTwo
                    |> Car.spawn ( 2, 1 )
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
                |> Car.spawn ( 1, 2 )
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
                |> Car.spawn ( 1, 2 )
                |> Car.turn Right
                |> Car.stopAtIntersection

        otherCars =
            [ carTwo
                |> Car.spawn ( 2, 1 )
                |> Car.turn Down
            ]
    in
    Round.new board False fakeRandomDirections car otherCars
