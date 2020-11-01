module SharedState exposing
    ( Cars
    , Lots
    , SharedState
    , SharedStateUpdate(..)
    , SimulationSpeed(..)
    , SimulationState(..)
    , addLot
    , hasLot
    , initial
    , isEmptyArea
    , nextId
    , setScreen
    , simulationSpeedValues
    , update
    )

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Collision
import Dict exposing (Dict)
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Lot exposing (Lot)
import Position exposing (Position)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )
import TrafficLight


type alias SharedState =
    { simulationState : SimulationState
    , screenSize : ( Int, Int )
    , board : Board
    , cars : Cars
    , lots : Lots
    }


type alias Cars =
    Dict Int Car


type alias Lots =
    Dict Int Lot


type SimulationSpeed
    = Slow
    | Medium
    | Fast


type SimulationState
    = Simulation SimulationSpeed
    | Paused


type SharedStateUpdate
    = NoUpdate
    | Replace SharedState
    | UpdateSimulationState SimulationState
    | UpdateBoard Board
    | UpdateCars Cars
    | NewBoard
    | EditBoardAt Cell Board
    | EditTileAt Cell Board


initial : SharedState
initial =
    -- Room for improvement: require screen size as parameter in order to avoid temporary values (zeros)
    { simulationState = Simulation Medium
    , screenSize = ( 0, 0 )
    , board = initialBoard
    , cars = Dict.empty
    , lots = Dict.empty
    }


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        Replace nextSharedState ->
            nextSharedState

        UpdateSimulationState state ->
            { sharedState | simulationState = state }

        UpdateBoard board ->
            { sharedState | board = board }

        UpdateCars cars ->
            { sharedState | cars = cars }

        NewBoard ->
            { sharedState
                | simulationState = Paused
                , cars = Dict.empty
                , lots = Dict.empty
                , board = Board.new
            }

        EditBoardAt cell nextBoard ->
            let
                nextLots =
                    Dict.filter
                        (\_ lot ->
                            Board.exists (Lot.anchorCell lot) nextBoard && not (Lot.inBounds cell lot)
                        )
                        sharedState.lots

                nextCars =
                    carsAfterBoardChange
                        { cell = cell
                        , nextLots = nextLots
                        , cars = sharedState.cars
                        }
            in
            { sharedState
                | board = nextBoard
                , cars = nextCars
                , lots = nextLots
            }

        EditTileAt cell nextBoard ->
            { sharedState
                | board = nextBoard
                , cars =
                    carsAfterBoardChange
                        { cell = cell
                        , nextLots = sharedState.lots
                        , cars = sharedState.cars
                        }
            }

        NoUpdate ->
            sharedState


nextId : Dict Int a -> Int
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1


carsAfterBoardChange :
    { cell : Cell
    , nextLots : Lots
    , cars : Cars
    }
    -> Cars
carsAfterBoardChange { cell, nextLots, cars } =
    cars
        -- Room for improvement: implement general orphan entity handling
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        True
            )
        -- Room for improvement: move the car back to it's lot instead
        |> Dict.map
            (\_ car ->
                if car.position == Cell.bottomLeftCorner cell then
                    Car.waitForRespawn car

                else
                    car
            )


hasLot : Lots -> Cell -> Bool
hasLot lots cell =
    List.any (Lot.inBounds cell) (Dict.values lots)


addLot : SharedState -> Lot -> SharedState
addLot sharedState lot =
    let
        { lots, cars } =
            sharedState

        nextLotId =
            nextId lots

        nextCarId =
            nextId cars

        newLots =
            Dict.insert nextLotId lot lots

        newCars =
            case Lot.resident lot of
                Just carKind ->
                    Dict.insert nextCarId
                        { kind = carKind
                        , position = Cell.bottomLeftCorner (Lot.entryCell lot)
                        , direction = Direction.next lot.content.entryDirection
                        , homeLotId = Just nextLotId
                        , status = Car.ParkedAtLot
                        }
                        cars

                _ ->
                    cars
    in
    { sharedState | lots = newLots, cars = newCars }


setScreen : ( Int, Int ) -> SharedState -> SharedState
setScreen ( width, height ) sharedState =
    { sharedState | screenSize = ( width, height ) }


isEmptyArea : { origin : Position, width : Float, height : Float } -> SharedState -> Bool
isEmptyArea { origin, width, height } sharedState =
    let
        roadBoundingBoxes =
            sharedState.board
                |> Dict.keys
                |> List.map Cell.boundingBox

        lotBoundingBoxes =
            sharedState.lots
                |> Dict.values
                |> List.map Lot.boundingBox

        areaBoundingBox =
            { x = Tuple.first origin, y = Tuple.second origin, width = width, height = height }

        inBoardBounds =
            Board.inBounds areaBoundingBox

        noCollision _ =
            not <| List.any (Collision.aabb areaBoundingBox) (roadBoundingBoxes ++ lotBoundingBoxes)
    in
    inBoardBounds && noCollision ()


simulationSpeedValues : SimulationSpeed -> ( Float, Float )
simulationSpeedValues speed =
    -- (Environment update, Traffic update)
    case speed of
        Slow ->
            ( 1200, 500 )

        Medium ->
            ( 900, 300 )

        Fast ->
            ( 600, 100 )


initialBoard : Dict ( Int, Int ) Tile
initialBoard =
    Dict.fromList
        [ ( ( 1, 1 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 4 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 1, 7 ), TwoLaneRoad (Curve TopLeft) Both )
        , ( ( 1, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 1, 10 ), TwoLaneRoad (Curve BottomLeft) Both )
        , ( ( 2, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 7 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 2, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 3, 4 ), Intersection (Yield Vertical) (T Down) )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 3, 7 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 3, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 4, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 5, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 1 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 6, 7 ), TwoLaneRoad (Regular Horizontal) OneWay )
        , ( ( 6, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 7, 1 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 7, 2 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 3 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 4 ), Intersection (Signal TrafficLight.default) Crossroads )
        , ( ( 7, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 7 ), Intersection (Stop Horizontal) (T Left) )
        , ( ( 7, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 7, 10 ), Intersection (Yield Vertical) (T Up) )
        , ( ( 8, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 8, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 4 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 9, 10 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 10, 4 ), TwoLaneRoad (Curve TopRight) Both )
        , ( ( 10, 5 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 6 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 7 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 8 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 9 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 10, 10 ), TwoLaneRoad (Curve BottomRight) Both )
        ]
