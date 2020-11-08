module World exposing
    ( Cars
    , Lots
    , SimulationSpeed(..)
    , SimulationState(..)
    , World
    , canBuildRoadAt
    , getCellContents
    , getRoadCells
    , hasLot
    , isEmptyArea
    , new
    , newWithInitialBoard
    , simulationSpeedValues
    , withBoard
    , withEmptyBoard
    , withEmptyCell
    , withNewLot
    , withNewRoad
    , withScreen
    , withSimulationState
    , withUpdatedCar
    , withUpdatedCell
    )

import Board exposing (Board)
import Car exposing (Car)
import Cell exposing (Cell)
import Collision
import Dict exposing (Dict)
import Dict.Extra as Dict
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


type alias World =
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


new : World
new =
    -- Room for improvement: require screen size as parameter in order to avoid temporary values (zeros)
    { simulationState = Simulation Medium
    , screenSize = ( 0, 0 )
    , board = Board.new
    , cars = Dict.empty
    , lots = Dict.empty
    }


newWithInitialBoard : World
newWithInitialBoard =
    new |> withBoard initialBoard



-- Internals


nextId : Dict Int a -> Int
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1



-- Modifications


withSimulationState : SimulationState -> World -> World
withSimulationState state world =
    { world | simulationState = state }


withScreen : ( Int, Int ) -> World -> World
withScreen ( width, height ) world =
    { world | screenSize = ( width, height ) }


withNewLot : Lot -> World -> World
withNewLot lot world =
    let
        { lots, cars } =
            world

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
    { world | lots = newLots, cars = newCars }


withUpdatedCar : Int -> Car -> World -> World
withUpdatedCar id car world =
    { world | cars = Dict.insert id car world.cars }


withBoard : Board -> World -> World
withBoard board world =
    { world | board = board }


withEmptyBoard : World -> World
withEmptyBoard world =
    { world
        | simulationState = Paused
        , cars = Dict.empty
        , lots = Dict.empty
        , board = Board.new
    }


withUpdatedCell : Cell -> Tile -> World -> World
withUpdatedCell cell tile world =
    { world
        | board = Dict.insert cell tile world.board
        , cars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = world.lots
                , cars = world.cars
                }
    }


withEmptyCell : Cell -> World -> World
withEmptyCell cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.remove cell
                |> Board.applyMask
        , world = world
        }


withNewRoad : Cell -> World -> World
withNewRoad cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.insert cell Board.defaultTile
                |> Board.applyMask
        , world = world
        }



-- Queries


getCellContents : Cell -> World -> Maybe Tile
getCellContents cell { board } =
    board
        |> Dict.find (\key _ -> key == cell)
        |> Maybe.map Tuple.second


getRoadCells : World -> List Cell
getRoadCells { board } =
    board
        |> Dict.filter
            (\_ t ->
                Tile.isRoad t
            )
        |> Dict.keys


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


canBuildRoadAt : Cell -> World -> Bool
canBuildRoadAt cell world =
    let
        withinAllowedComplexity l =
            List.length l < 3

        hasLowComplexity corner =
            Cell.cornerAndNeighbors corner cell
                |> List.filterMap (\c -> getCellContents c world)
                |> withinAllowedComplexity
    in
    List.all hasLowComplexity Direction.corners


isEmptyArea : { origin : Position, width : Float, height : Float } -> World -> Bool
isEmptyArea { origin, width, height } world =
    let
        roadBoundingBoxes =
            world.board
                |> Dict.keys
                |> List.map Cell.boundingBox

        lotBoundingBoxes =
            world.lots
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



-- Utility


worldAfterBoardChange : { cell : Cell, nextBoard : Board, world : World } -> World
worldAfterBoardChange { cell, nextBoard, world } =
    let
        nextLots =
            Dict.filter
                (\_ lot ->
                    Board.exists (Lot.anchorCell lot) nextBoard && not (Lot.inBounds cell lot)
                )
                world.lots

        nextCars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = nextLots
                , cars = world.cars
                }
    in
    { world
        | board = nextBoard
        , cars = nextCars
        , lots = nextLots
    }


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
