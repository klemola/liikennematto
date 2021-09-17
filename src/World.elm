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
    , setCar
    , spawnCar
    , tileAt
    )

import Board exposing (Board, Tile)
import BoundingBox2d
import Car exposing (Car, CarKind(..), Cars)
import Cell exposing (Cell)
import Color
import Dict
import Dict.Extra as Dict
import Direction2d
import Entity exposing (Id)
import Geometry exposing (LMBoundingBox2d)
import Lot exposing (BuildingKind(..), Lot, Lots)
import Random
import RoadNetwork exposing (RNLookupTree, RNNodeContext, RoadNetwork)
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
        ( roadNetwork, trafficLights ) =
            RoadNetwork.fromBoardAndLots initialBoard empty.lots
                |> RoadNetwork.setupTrafficControl empty.trafficLights
    in
    { empty
        | board = initialBoard
        , roadNetwork = roadNetwork
        , trafficLights = trafficLights
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

        ( roadNetwork, nextTrafficLights ) =
            RoadNetwork.fromBoardAndLots world.board nextLots
                |> RoadNetwork.setupTrafficControl world.trafficLights
    in
    { world
        | lots = nextLots
        , roadNetwork = roadNetwork
        , trafficLights = nextTrafficLights
    }
        |> addLotResident nextLotId lot


addLotResident : Int -> Lot -> World -> World
addLotResident lotId lot world =
    let
        carId =
            Entity.nextId world.cars

        createCar kind =
            Car.new kind
                |> Car.withHome lotId
                |> Car.withPosition lot.entryDetails.parkingSpot
                |> Car.withOrientation (Lot.parkingSpotOrientation lot)
                |> Car.build carId Nothing

        addToWorld car =
            { world | cars = Dict.insert carId car world.cars }
    in
    world.lots
        |> Dict.get lotId
        |> Maybe.andThen resident
        |> Maybe.map (createCar >> addToWorld)
        |> Maybe.withDefault world


spawnCar : Random.Seed -> World -> ( World, Random.Seed, Maybe Entity.Id )
spawnCar seed world =
    let
        ( maybeRandomNodeCtx, nextSeed ) =
            RoadNetwork.getRandomNode world.roadNetwork seed
    in
    maybeRandomNodeCtx
        |> Maybe.andThen (validateSpawnConditions world)
        |> Maybe.map
            (\nodeCtx ->
                let
                    id =
                        Entity.nextId world.cars

                    nextNode =
                        RoadNetwork.getOutgoingConnections nodeCtx
                            |> List.head
                            |> Maybe.andThen (RoadNetwork.findNodeByNodeId world.roadNetwork)

                    car =
                        Car.new Car.TestCar
                            |> Car.withPosition nodeCtx.node.label.position
                            |> Car.withOrientation (Direction2d.toAngle nodeCtx.node.label.direction)
                            |> Car.build id nextNode
                            |> Car.startMoving
                in
                ( { world | cars = Dict.insert id car world.cars }
                , nextSeed
                , Just id
                )
            )
        |> Maybe.withDefault ( world, nextSeed, Nothing )


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

        ( nextRoadNetwork, nextTrafficLights ) =
            RoadNetwork.fromBoardAndLots nextBoard nextLots
                |> RoadNetwork.setupTrafficControl world.trafficLights

        nextCars =
            carsAfterBoardChange
                { cell = cell
                , currentCars = world.cars
                , nextLots = nextLots
                , nextRoadNetwork = nextRoadNetwork
                }
    in
    { world
        | board = nextBoard
        , roadNetwork = nextRoadNetwork
        , cars = nextCars
        , lots = nextLots
        , trafficLights = nextTrafficLights
    }


carsAfterBoardChange :
    { cell : Cell
    , currentCars : Cars
    , nextLots : Lots
    , nextRoadNetwork : RoadNetwork
    }
    -> Cars
carsAfterBoardChange { nextLots, currentCars, nextRoadNetwork } =
    let
        connectionLookup =
            RoadNetwork.lookupTree nextRoadNetwork
    in
    currentCars
        |> Dict.filter
            (\_ car ->
                case car.homeLotId of
                    Just lotId ->
                        Dict.member lotId nextLots

                    Nothing ->
                        car.kind == TestCar
            )
        |> Dict.map
            (\_ car ->
                case car.status of
                    Car.Moving ->
                        updateRoute nextRoadNetwork connectionLookup car

                    _ ->
                        car
            )


updateRoute : RoadNetwork -> RNLookupTree -> Car -> Car
updateRoute roadNetwork connectionLookup car =
    case
        List.head car.route
            |> Maybe.andThen (RoadNetwork.findNodeReplacement connectionLookup)
            |> Maybe.andThen (RoadNetwork.findNodeByNodeId roadNetwork)
    of
        Just nodeCtxResult ->
            Car.createRoute nodeCtxResult car

        Nothing ->
            Car.markAsConfused car


validateSpawnConditions : World -> RNNodeContext -> Maybe RNNodeContext
validateSpawnConditions world nodeCtx =
    let
        notAtSpawnPosition car =
            car.boundingBox
                |> BoundingBox2d.contains nodeCtx.node.label.position
                |> not

        reasonableAmountOfTraffic =
            Dict.size world.board > Dict.size world.cars

        spawnPositionHasEnoughSpace =
            Dict.values world.cars
                |> List.all notAtSpawnPosition
    in
    if reasonableAmountOfTraffic && spawnPositionHasEnoughSpace then
        Just nodeCtx

    else
        Nothing



--
-- Data
--


resident : Lot -> Maybe CarKind
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just <|
                Sedan
                    { body = Color.rgb255 47 149 208
                    , detail = Color.rgb255 41 141 198
                    , shade = Color.rgb255 208 147 173
                    , edge = Color.rgb255 22 98 142
                    }

        ResidentialB ->
            Just <|
                Sedan
                    { body = Color.rgb255 232 106 23
                    , detail = Color.rgb255 191 100 40
                    , shade = Color.rgb255 217 163 125
                    , edge = Color.rgb255 159 73 16
                    }

        ResidentialC ->
            Just <|
                Sedan
                    { body = Color.rgb255 255 204 0
                    , detail = Color.rgb255 229 186 16
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 159 128 10
                    }

        ResidentialD ->
            Just <|
                Sedan
                    { body = Color.rgb255 57 194 114
                    , detail = Color.rgb255 48 182 104
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 20 119 61
                    }

        ResidentialE ->
            Just <|
                Sedan
                    { body = Color.rgb255 93 91 91
                    , detail = Color.rgb255 79 76 76
                    , shade = Color.rgb255 208 184 147
                    , edge = Color.rgb255 58 53 53
                    }

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
