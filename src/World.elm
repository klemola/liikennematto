module World exposing
    ( World
    , addLot
    , buildRoadAt
    , canBuildRoadAt
    , default
    , empty
    , hasLot
    , hasLotAnchor
    , isEmptyArea
    , removeRoadAt
    , reset
    , setCar
    , spawnCar
    , tileAt
    )

import Board exposing (Board, Tile)
import Car exposing (Car, CarKind(..), Cars)
import Cell exposing (Cell, Corner(..), OrthogonalDirection(..))
import Config exposing (acceleration, maxVelocity)
import Dict
import Dict.Extra as Dict
import Direction2d
import Entity exposing (Id)
import Geometry exposing (LMBoundingBox2d)
import Lot exposing (BuildingKind(..), Lot, Lots)
import Quantity
import RoadNetwork exposing (RNNodeContext, RoadNetwork)
import TrafficLight exposing (TrafficLights)


type alias World =
    { board : Board
    , roadNetwork : RoadNetwork
    , trafficLights : TrafficLights
    , cars : Cars
    , lots : Lots
    }



--
-- Presets
--


empty : World
empty =
    { board = Dict.empty
    , roadNetwork = RoadNetwork.new
    , trafficLights = Dict.empty
    , cars = Dict.empty
    , lots = Dict.empty
    }


default : World
default =
    let
        testTrafficLightId =
            Entity.nextId empty.trafficLights

        testTrafficLight =
            TrafficLight.new
                |> TrafficLight.withPosition (Geometry.pointFromPosition { x = 80, y = 80 })
                |> TrafficLight.build testTrafficLightId
    in
    { empty
        | board = initialBoard
        , roadNetwork = RoadNetwork.fromBoardAndLots initialBoard empty.lots
        , trafficLights =
            empty.trafficLights
                |> Dict.insert testTrafficLightId testTrafficLight
    }



--
-- Modifications
--


addLot : Lot -> World -> World
addLot lot world =
    let
        nextLotId =
            Entity.nextId world.lots

        nextLots =
            Dict.insert nextLotId lot world.lots
    in
    { world
        | lots = nextLots
        , roadNetwork = RoadNetwork.fromBoardAndLots world.board nextLots
    }
        |> addLotResident nextLotId


addLotResident : Int -> World -> World
addLotResident lotId world =
    let
        carId =
            Entity.nextId world.cars

        createCar kind =
            Car.new kind
                |> Car.withHome lotId
                |> Car.build carId
                |> moveCarToHome world

        addToWorld car =
            { world | cars = Dict.insert carId car world.cars }
    in
    world.lots
        |> Dict.get lotId
        |> Maybe.andThen resident
        |> Maybe.map (createCar >> addToWorld)
        |> Maybe.withDefault world


spawnCar : World -> RNNodeContext -> World
spawnCar world nodeCtx =
    let
        id =
            Entity.nextId world.cars

        car =
            Car.new Car.TestCar
                |> Car.withPosition nodeCtx.node.label.position
                |> Car.withRotation (Direction2d.toAngle nodeCtx.node.label.direction)
                |> Car.withVelocity maxVelocity
                |> Car.build id

        withRoute =
            Car.buildRoute car nodeCtx
    in
    { world | cars = Dict.insert id withRoute world.cars }


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
        | cars = empty.cars
        , lots = empty.lots
        , board = empty.board
        , roadNetwork = empty.roadNetwork
    }



--
-- Queries
--


tileAt : Cell -> World -> Maybe Tile
tileAt cell { board } =
    board
        |> Dict.find (\key _ -> key == cell)
        |> Maybe.map Tuple.second


hasLot : Cell -> World -> Bool
hasLot cell { lots } =
    List.any (Lot.inBounds cell) (Dict.values lots)


hasLotAnchor : Cell -> World -> Bool
hasLotAnchor cell { lots } =
    List.any (\lot -> Lot.anchorCell lot == cell) (Dict.values lots)


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
    List.all hasLowComplexity Cell.corners


isEmptyArea : LMBoundingBox2d -> World -> Bool
isEmptyArea testAreaBB world =
    let
        roadBoundingBoxes =
            world.board
                |> Dict.keys
                |> List.map Cell.boundingBox

        lotBoundingBoxes =
            world.lots
                |> Dict.values
                |> List.map Lot.boundingBox

        inBoardBounds =
            Board.inBounds testAreaBB

        noCollision _ =
            (roadBoundingBoxes ++ lotBoundingBoxes)
                |> List.all (Geometry.noBoundingBoxOverlap testAreaBB)
    in
    inBoardBounds && noCollision ()


hasValidAnchorCell : Board -> Lot -> Bool
hasValidAnchorCell board lot =
    case Dict.get (Lot.anchorCell lot) board of
        Just tile ->
            tile == Board.twoLaneRoadHorizontal || tile == Board.twoLaneRoadVertical

        Nothing ->
            False



--
-- Utility
--


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
carsAfterBoardChange { nextLots, world } =
    world.cars
        -- Room for improvement: implement general orphan entity handling
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        False
            )
        |> Dict.map (\_ car -> moveCarToHome world car)


moveCarToHome : World -> Car -> Car
moveCarToHome world car =
    let
        home =
            car.homeLotId
                |> Maybe.andThen (\lotId -> Dict.get lotId world.lots)

        nextCarBase =
            car.homeLotId
                |> Maybe.andThen (RoadNetwork.findNodeByLotId world.roadNetwork)
                |> Maybe.map (Car.buildRoute car)
                |> Maybe.withDefault car
    in
    case home of
        Just lot ->
            { nextCarBase
                | position = Lot.parkingSpot lot
                , rotation =
                    lot.content.entryDirection
                        |> Cell.orthogonalDirectionToLmDirection
                        |> Direction2d.rotateClockwise
                        |> Direction2d.toAngle
                , status = Car.ParkedAtLot
                , velocity = Quantity.zero
                , acceleration = acceleration.speedUp
            }

        Nothing ->
            Car.markAsConfused car



--
-- Data
--


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
    Dict.empty
        |> Dict.insert ( 2, 5 ) Board.defaultTile
        |> Dict.insert ( 3, 5 ) Board.defaultTile
        |> Dict.insert ( 4, 5 ) Board.defaultTile
        |> Dict.insert ( 5, 3 ) Board.defaultTile
        |> Dict.insert ( 5, 4 ) Board.defaultTile
        |> Dict.insert ( 5, 5 ) Board.defaultTile
        |> Board.applyMask
