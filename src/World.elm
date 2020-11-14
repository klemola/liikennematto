module World exposing
    ( Cars
    , Lots
    , SimulationSpeed(..)
    , SimulationState(..)
    , World
    , buildRoadAt
    , canBuildRoadAt
    , hasLot
    , isEmptyArea
    , new
    , newWithInitialBoard
    , removeRoadAt
    , reset
    , roadCells
    , setCar
    , simulationSpeedValues
    , tileAt
    , withBoard
    , withCar
    , withLot
    , withScreen
    , withSimulationState
    , withTileAt
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


type alias Id =
    Int


type alias Cars =
    Dict Id Car


type alias Lots =
    Dict Id Lot


type SimulationSpeed
    = Slow
    | Medium
    | Fast


type SimulationState
    = Simulation SimulationSpeed
    | Paused


new : World
new =
    { simulationState = Simulation Medium
    , screenSize = ( 0, 0 )
    , board = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    }


newWithInitialBoard : World
newWithInitialBoard =
    new |> withBoard initialBoard



-- Internals


nextId : Dict Id a -> Id
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


withLot : Lot -> World -> World
withLot lot world =
    let
        { lots, cars } =
            world

        nextLotId =
            nextId lots

        worldWithNewLot =
            { world | lots = Dict.insert nextLotId lot lots }
    in
    Lot.resident lot
        |> Maybe.map
            (\carKind ->
                worldWithNewLot
                    |> withCar
                        { kind = carKind
                        , position = Cell.bottomLeftCorner (Lot.entryCell lot)
                        , direction = Direction.next lot.content.entryDirection
                        , homeLotId = Just nextLotId
                        , status = Car.ParkedAtLot
                        }
            )
        |> Maybe.withDefault worldWithNewLot


withCar : Car -> World -> World
withCar car world =
    { world | cars = Dict.insert (nextId world.cars) car world.cars }


withBoard : Board -> World -> World
withBoard board world =
    { world | board = board }


withTileAt : Cell -> Tile -> World -> World
withTileAt cell tile world =
    { world
        | board = Dict.insert cell tile world.board
        , cars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = world.lots
                , cars = world.cars
                }
    }


setCar : Id -> Car -> World -> World
setCar id car world =
    { world | cars = Dict.insert id car world.cars }


buildRoadAt : Cell -> World -> World
buildRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.insert cell Board.defaultTile
                |> Board.applyMask
        , world = world
        }


removeRoadAt : Cell -> World -> World
removeRoadAt cell world =
    worldAfterBoardChange
        { cell = cell
        , nextBoard =
            world.board
                |> Dict.remove cell
                |> Board.applyMask
        , world = world
        }


reset : World -> World
reset world =
    { world
        | cars = new.cars
        , lots = new.lots
        , board = new.board
    }



-- Queries


tileAt : Cell -> World -> Maybe Tile
tileAt cell { board } =
    board
        |> Dict.find (\key _ -> key == cell)
        |> Maybe.map Tuple.second


roadCells : World -> List Cell
roadCells { board } =
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
                |> List.filterMap (\c -> tileAt c world)
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
        [ ( ( 2, 5 ), TwoLaneRoad (Deadend Left) Both )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 3 ), TwoLaneRoad (Deadend Up) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 5, 5 ), TwoLaneRoad (Curve BottomRight) Both )
        ]
