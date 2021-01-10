module World exposing
    ( Cars
    , Lots
    , SimulationState(..)
    , World
    , buildRoadAt
    , canBuildRoadAt
    , hasConnectedRoad
    , hasLot
    , hasLotAnchor
    , isEmptyArea
    , new
    , newWithInitialBoard
    , removeRoadAt
    , reset
    , roadCells
    , setCar
    , tileAt
    , withBoard
    , withLot
    , withScreen
    , withSimulationState
    , withTileAt
    )

import Board exposing (Board)
import Car exposing (Car, CarKind(..))
import Cell exposing (Cell)
import Collision
import Dict exposing (Dict)
import Dict.Extra as Dict
import Direction exposing (Corner(..), Direction(..), Orientation(..))
import Lot exposing (BuildingKind(..), Lot)
import Position exposing (Position)
import RoadNetwork exposing (RoadNetwork)
import Tile
    exposing
        ( IntersectionControl(..)
        , IntersectionShape(..)
        , RoadKind(..)
        , Tile(..)
        , TrafficDirection(..)
        )


type alias World =
    { simulationState : SimulationState
    , screenSize : ( Int, Int )
    , board : Board
    , roadNetwork : RoadNetwork
    , cars : Cars
    , lots : Lots
    }


type alias Id =
    Int


type alias Cars =
    Dict Id Car


type alias Lots =
    Dict Id Lot


type SimulationState
    = RunningAtNormalSpeed
    | RunningAtSlowSpeed
    | Paused


new : World
new =
    { simulationState = RunningAtNormalSpeed
    , screenSize = ( 0, 0 )
    , board = Dict.empty
    , roadNetwork = RoadNetwork.new
    , cars = Dict.empty
    , lots = Dict.empty
    }


newWithInitialBoard : World
newWithInitialBoard =
    new
        |> withBoard initialBoard
        |> withRoadConnections



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
        nextLotId =
            nextId world.lots

        nextLots =
            Dict.insert nextLotId lot world.lots

        worldWithNewLot =
            { world
                | lots = nextLots
                , roadNetwork = RoadNetwork.fromBoardAndLots world.board nextLots
            }

        -- Room for improvement: create the resident/car separately
        createCar carKind =
            worldWithNewLot
                |> withCar
                    (Car.new carKind
                        |> Car.withHome nextLotId
                        |> moveCarToHome worldWithNewLot
                    )
    in
    resident lot
        |> Maybe.map createCar
        |> Maybe.withDefault worldWithNewLot


withCar : Car -> World -> World
withCar car world =
    { world | cars = Dict.insert (nextId world.cars) car world.cars }


withBoard : Board -> World -> World
withBoard board world =
    { world | board = board }


withRoadConnections : World -> World
withRoadConnections world =
    { world | roadNetwork = RoadNetwork.fromBoardAndLots world.board world.lots }


withTileAt : Cell -> Tile -> World -> World
withTileAt cell tile world =
    { world
        | board = Dict.insert cell tile world.board
        , cars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = world.lots
                , world = world
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
        , roadNetwork = new.roadNetwork
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


hasLotAnchor : Cell -> World -> Bool
hasLotAnchor cell { lots } =
    List.any (\lot -> Lot.anchorCell lot == cell) (Dict.values lots)


hasConnectedRoad : { currentCell : Cell, direction : Direction, world : World } -> Bool
hasConnectedRoad { currentCell, direction, world } =
    case
        ( tileAt currentCell world
        , tileAt (Cell.next direction currentCell) world
        )
    of
        ( Just current, Just next ) ->
            Tile.connected direction current next

        ( Nothing, Just next ) ->
            hasLot currentCell world

        _ ->
            False


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


hasValidAnchorCell : Board -> Lot -> Bool
hasValidAnchorCell board lot =
    case Dict.get (Lot.anchorCell lot) board of
        Just (TwoLaneRoad (Regular Vertical) _) ->
            True

        Just (TwoLaneRoad (Regular Horizontal) _) ->
            True

        _ ->
            False



-- Utility


worldAfterBoardChange : { cell : Cell, nextBoard : Board, world : World } -> World
worldAfterBoardChange { cell, nextBoard, world } =
    let
        nextLots =
            Dict.filter
                (\_ lot ->
                    hasValidAnchorCell nextBoard lot && not (Lot.inBounds cell lot)
                )
                world.lots

        nextCars =
            carsAfterBoardChange
                { cell = cell
                , nextLots = nextLots
                , world = world
                }

        roadNetwork =
            RoadNetwork.fromBoardAndLots nextBoard world.lots
    in
    { world
        | board = nextBoard
        , roadNetwork = roadNetwork
        , cars = nextCars
        , lots = nextLots
    }


carsAfterBoardChange :
    { cell : Cell
    , nextLots : Lots
    , world : World
    }
    -> Cars
carsAfterBoardChange { cell, nextLots, world } =
    world.cars
        -- Room for improvement: implement general orphan entity handling
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        True
            )
        |> Dict.map (\_ car -> moveCarToHome world car)


moveCarToHome : World -> Car -> Car
moveCarToHome world car =
    let
        home =
            car.homeLotId
                |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)

        route =
            car.homeLotId
                |> Maybe.andThen (RoadNetwork.findNodeByLotId world.roadNetwork)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    case home of
        Just lot ->
            { car
                | position = Lot.parkingSpot lot
                , rotation =
                    lot.content.entryDirection
                        |> Direction.next
                        |> Direction.toRadians
                , status = Car.ParkedAtLot
                , route = route
            }

        Nothing ->
            Car.markAsConfused car



-- Data


resident : Lot -> Maybe CarKind
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just SedanA

        ResidentialB ->
            Just SedanB

        ResidentialC ->
            Just SedanC

        ResidentialD ->
            Just SedanD

        ResidentialE ->
            Just SedanE

        _ ->
            Nothing


initialBoard : Board
initialBoard =
    Dict.fromList
        [ ( ( 2, 5 ), TwoLaneRoad (Deadend Left) Both )
        , ( ( 3, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 4, 5 ), TwoLaneRoad (Regular Horizontal) Both )
        , ( ( 5, 3 ), TwoLaneRoad (Deadend Up) Both )
        , ( ( 5, 4 ), TwoLaneRoad (Regular Vertical) Both )
        , ( ( 5, 5 ), TwoLaneRoad (Curve BottomRight) Both )
        ]
